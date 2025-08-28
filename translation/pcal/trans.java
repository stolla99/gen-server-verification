package pcal;

import io.github.cdimascio.dotenv.Dotenv;
import kotlin.Pair;
import org.apache.commons.io.FileUtils;
import org.jetbrains.annotations.Nullable;
import pcal.ValidationCallBack.Generate;
import pcal.exception.*;
import pcal.postprocessor.Typer;
import util.TLAConstants;
import util.ToolIO;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Vector;
import java.util.regex.Matcher;
public class trans {
    static final boolean disableErlangTranslation = false;
    static final boolean skipStartFunctions = true;

    /**
     * Status indicating no errors and successful process
     */
    static final int STATUS_OK = 1;
    /**
     * Status of no errors, but abort of the translation
     */
    static final int STATUS_EXIT_WITHOUT_ERROR = 0;
    /**
     * Status of present errors and abort of the translation
     */
    static final int STATUS_EXIT_WITH_ERRORS = -1;

    private static final String PCAL_TRANSLATION_COMMENT_LINE_PREFIX = "\\* " + PcalParams.BeginXlation1 + " " + PcalParams.BeginXlation2;
    private static final String TLA_TRANSLATION_COMMENT_LINE_PREFIX = "\\* " + PcalParams.EndXlation1 + " " + PcalParams.EndXlation2;

    // Erlang target destination different from source directory, good to keep translator and erlang out files separate
    public static String ERLANG_TARGET = "";
    public static Boolean ERLANG_TAKE_PARENT = false;

    /**
     * @param args, command line arguments
     */
    public static void main(String[] args) {
        Dotenv dotenv = Dotenv.configure().directory(".env/").filename(".env.local").load();
        ERLANG_TARGET = dotenv.get("ERLANG_TARGET");
        ERLANG_TAKE_PARENT = Boolean.parseBoolean(dotenv.get("ERLANG_TAKE_PARENT"));
        TLAConstants.RELIABLE_NETWORK = Boolean.parseBoolean(dotenv.get("USE_RELIABLE_NETWORK"));
        System.exit(runMe(new String[]{!Objects.requireNonNull(dotenv.get("ARG")).isBlank() ? dotenv.get("ARG") : "", dotenv.get("PARAMETER")}));
    }

    /**
     * The main translator method
     *
     * @return one of {@link trans#STATUS_OK}, {@link trans#STATUS_EXIT_WITH_ERRORS},
     * {@link trans#STATUS_EXIT_WITH_ERRORS}
     * indicating the status
     * <p>
     * Modified by LL on 16 Dec 2011.  Changed the return value to the
     * TLAtoPCalMapping object for the translation.  (The status return
     * value was not being used.)  If the translation fails, it returns
     * null.
     */
    public static int runMe(String[] args) {
        if (ToolIO.getMode() == ToolIO.SYSTEM) {
            PcalDebug.reportInfo("pcal.trans Version " + PcalParams.version + " of " + PcalParams.modDate);
        }
        PcalParams.resetParams();
        int status = parseAndProcessArguments(args);
        if (status != STATUS_OK) {
            return exitWithStatus(status);
        }
        List<String> inputVec;
        try {
            inputVec = fileToStringVector(PcalParams.TLAInputFile + TLAConstants.Files.TLA_EXTENSION);
        } catch (FileToStringVectorException e) {
            PcalDebug.reportError(e);
            return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
        }

        final TranslationResult transRes = performTranslation(inputVec);
        if (transRes == null) {
            return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
        }

        final List<String> tlaVec = transRes.getTlaCode();
        List<String> erlangVec = transRes.getErlangCode();
        final List<String> hrlVec = transRes.getHrlCode();
        boolean hasTranslationErrors = tlaVec == null;

        if (PcalParams.TranslateErlang) {
            hasTranslationErrors = hasTranslationErrors || erlangVec == null || hrlVec == null;
        }

        if (!disableErlangTranslation && hasTranslationErrors) {
            return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
        }

        boolean renameToOld = !PcalParams.NoOld;
        if (renameToOld) {
            File file;
            try {
                file = new File(PcalParams.TLAInputFile + ".old");
                if (file.exists()) {
                    file.delete();
                }
                file = new File(PcalParams.TLAInputFile + TLAConstants.Files.TLA_EXTENSION);
                file.renameTo(new File(PcalParams.TLAInputFile + ".old"));
            } catch (Exception e) {
                PcalDebug.reportError("Could not rename input file " + PcalParams.TLAInputFile + TLAConstants.Files.TLA_EXTENSION + " to " + PcalParams.TLAInputFile + ".old");
                return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
            }
        }

        String fileAndPath;
        try {
            fileAndPath = PcalParams.TLAInputFile + TLAConstants.Files.TLA_EXTENSION;
            WriteStringVectorToFile(tlaVec, fileAndPath);
        } catch (StringVectorToFileException e) {
            PcalDebug.reportError(e);
            return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
        }

        PcalDebug.reportWrite("      📝 New file " + fileAndPath + " written.");

        if (PcalParams.TranslateErlang) {
            List<String> erlaLibsNames = PcalErlangGen.getErlaLibsModuleNames();
            for (String erlaLibsName : erlaLibsNames) {
                try {
                    String erlaLibsFileName = erlaLibsName + ".erl";
                    Path parentPath = Paths.get(PcalParams.TLAInputFile).getParent();
                    String parentName;
                    if (parentPath == null) {
                        parentName = "";
                    } else {
                        parentName = parentPath.toString();
                    }
                    Path destination = Paths.get(parentName, erlaLibsFileName);
                    Path origin = Paths.get("../src/pcal/erla_libs/" + erlaLibsFileName);
                    if (!Files.exists(origin)) {
                        PcalDebug.reportWarning(erlaLibsFileName + " not found");
                    } else {
                        Files.copy(origin, destination, StandardCopyOption.REPLACE_EXISTING);
                        String outputDir = "";
                        if (!parentName.isEmpty()) {
                            outputDir = parentName + File.separator;
                        }
                        outputDir += erlaLibsFileName;
                        PcalDebug.reportWrite("      📝 New file " + outputDir + " written.");
                    }
                } catch (Exception e) {
                    PcalDebug.reportWarning("Error in " + erlaLibsName + " generation");
                }
            }

            String erlangFileName = PcalErlangGen.getErlangFileNameOnly();
            String hrlFileName = PcalErlangGen.getHrlFileNameOnly();
            String path = ERLANG_TAKE_PARENT ? ERLANG_TARGET : "";
            String erlangCodeFileNameWithPath = path + File.separator + erlangFileName;
            String hrlCodeFileNameWithPath = path + File.separator + hrlFileName;
            Integer STATUS_EXIT_WITH_ERRORS1 = writeToFileAndGetExitCode(erlangVec, erlangCodeFileNameWithPath);
            Integer STATUS_EXIT_WITH_ERRORS2 = writeToFileAndGetExitCode(hrlVec, hrlCodeFileNameWithPath);
            if (STATUS_EXIT_WITH_ERRORS1 != null && STATUS_EXIT_WITH_ERRORS2 != null) return STATUS_EXIT_WITH_ERRORS1;

            // Postprocessing
            Typer typerFlow = new Typer();
            assert erlangVec != null;
            erlangVec = typerFlow.executePostprocessing(new Vector<>(erlangVec));
            PcalDebug.reportInfo("      📝 Overwritten file " + erlangCodeFileNameWithPath);
            STATUS_EXIT_WITH_ERRORS1 = writeToFileAndGetExitCode(erlangVec, erlangCodeFileNameWithPath);
            if (STATUS_EXIT_WITH_ERRORS1 != null) return STATUS_EXIT_WITH_ERRORS1;
        }

        final File cfgFile = new File(PcalParams.TLAInputFile + TLAConstants.Files.CONFIG_EXTENSION);
        List<String> cfg = null;
        boolean writeCfg = !PcalParams.Nocfg;
        if (writeCfg && cfgFile.exists()) {
            if (cfgFile.canRead()) {
                try {
                    cfg = fileToStringVector(PcalParams.TLAInputFile + TLAConstants.Files.CONFIG_EXTENSION);
                } catch (FileToStringVectorException e) {
                    PcalDebug.reportError(e);
                    return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
                }
            } else {
                writeCfg = false;
                PcalDebug.reportInfo("File " + PcalParams.TLAInputFile + ".cfg is read only, new version not written.");
            }
        } else {
            cfg = new ArrayList<>();
            cfg.add(PcalParams.CfgFileDelimiter);
        }

        if (writeCfg) {
            int j = 0;
            boolean done = false;
            while ((!done) && (cfg.size() > j)) {
                if (cfg.get(j).indexOf(PcalParams.CfgFileDelimiter) == -1) {
                    j = j + 1;
                } else {
                    done = true;
                }
            }
            if (done) {
                while (j > 0) {
                    cfg.remove(0);
                    j = j - 1;
                }
            } else {
                cfg.add(0, PcalParams.CfgFileDelimiter);
            }

            if (PcalParams.tlcTranslation() || ParseAlgorithm.hasDefaultInitialization) {
                cfg.add(0, "CONSTANT defaultInitValue = defaultInitValue");
            }
            if (PcalParams.CheckTermination) {
                cfg.add(0, "PROPERTY Termination");
            }

            boolean hasSpec = false;
            for (final String thisLine : cfg) {
                if ((thisLine.contains(TLAConstants.KeyWords.SPECIFICATION)) && ((!thisLine.contains("\\*")) || (thisLine.indexOf("\\*") > thisLine.indexOf(TLAConstants.KeyWords.SPECIFICATION)))) {
                    hasSpec = true;
                    break;
                }
            }

            if (hasSpec) {
                PcalDebug.reportInfo("      📝 File " + PcalParams.TLAInputFile + ".cfg already contains " + TLAConstants.KeyWords.SPECIFICATION + " statement, so new one not written.");
            } else {
                cfg.add(0, TLAConstants.KeyWords.SPECIFICATION + " Spec");
            }

            try {
                WriteStringVectorToFile(cfg, PcalParams.TLAInputFile + TLAConstants.Files.CONFIG_EXTENSION);
            } catch (StringVectorToFileException e) {
                PcalDebug.reportError(e);
                return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
            }
            PcalDebug.reportWrite("      📝 New file " + PcalParams.TLAInputFile + TLAConstants.Files.CONFIG_EXTENSION + " written.");
        }

        return exitWithStatus(STATUS_EXIT_WITHOUT_ERROR);
    } // END main

    @Nullable
    private static Integer writeToFileAndGetExitCode(List<String> erlangVec, String erlangCodeFileNameWithPath) {
        try {
            WriteStringVectorToFile(erlangVec, erlangCodeFileNameWithPath);
        } catch (StringVectorToFileException e) {
            PcalDebug.reportError(e);
            return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
        }
        PcalDebug.reportWrite("      📝 New file " + erlangCodeFileNameWithPath + " written.");
        return null;
    }

    // This is called from the main-invoked {@link #runMe(String[])}
    // For some reason this method used to both mutate the argument, and then also returns that argument... ?
    // Now we copy the argument, mutate the copy, and return that.
    public static TranslationResult performTranslation(final List<String> specificationText) {
        return performTranslation(specificationText, new ValidationCallBack.Noop());
    }

    public static TranslationResult performTranslation(final List<String> specificationText, ValidationCallBack cb) {
        final TLAtoPCalMapping mapping = new TLAtoPCalMapping();
        PcalParams.tlaPcalMapping = mapping;

        final Vector<String> untabInputVec = removeTabs(specificationText);
        IntPair searchLoc = new IntPair(0, 0);
        boolean notDone = true;
        while (notDone) {
            try {
                ParseAlgorithm.FindToken("PlusCal", untabInputVec, searchLoc, "");
                String line = ParseAlgorithm.GotoNextNonSpace(untabInputVec, searchLoc);
                String restOfLine = line.substring(searchLoc.two);
                if (restOfLine.startsWith("options")) {
                    // The first string after "PlusCal" not starting with a
                    // space character is "options"
                    if (ParseAlgorithm.NextNonIdChar(restOfLine, 0) == 7) {
                        // The "options" should begin an options line
                        PcalParams.optionsInFile = true;
                        ParseAlgorithm.ProcessOptions(untabInputVec, searchLoc);
                        notDone = false;
                    }
                }
            } catch (ParseAlgorithmException e) {
                // The token "PlusCal" not found.
                notDone = false;
            }
        }
        int translationLine = 0;
        int algLine = 0;
        int algCol = -1;
        final ArrayList<String> tlaOutput = new ArrayList<>(specificationText);

        translationLine = findTokenPair(untabInputVec, 0, PcalParams.BeginXlation1, PcalParams.BeginXlation2);
        int endTranslationLine = -1;
        if (translationLine != -1) {
            endTranslationLine = findTokenPair(untabInputVec, translationLine + 1, PcalParams.EndXlation1, PcalParams.EndXlation2);
            if (endTranslationLine == -1) {
                PcalDebug.reportError("No line containing `" + PcalParams.EndXlation1 + " " + PcalParams.EndXlation2);
                return null;
            }
            int etl = endTranslationLine - 1;
            while (translationLine < etl) {
                tlaOutput.remove(etl);
                untabInputVec.remove(etl);
                etl--;
            }
        }

        // Search for "--algorithm" or "--fair".
        // If found set algLine and algCol right after the last char,
        // set foundBegin true, and set foundFairBegin true iff it
        // was "--fair".  Otherwise, set foundBegin false.
        boolean foundBegin = false;
        boolean foundFairBegin = false;
        while ((algLine < untabInputVec.size()) && !foundBegin) {
            String line = untabInputVec.elementAt(algLine);
            algCol = line.indexOf(PcalParams.BeginAlg);
            if (algCol != -1) {
                algCol = algCol + PcalParams.BeginAlg.length();
                foundBegin = true;
            } else {
                algCol = line.indexOf(PcalParams.BeginFairAlg);
                if (algCol != -1) {
                    // Found the "--fair".  The more structurally nice thing to
                    // do here would be to move past the following "algorithm".
                    // However, it's easier to pass a parameter to the ParseAlgorithm
                    // class's GetAlgorithm method that tells it to go past the
                    // "algorithm" token.
                    algCol = algCol + PcalParams.BeginFairAlg.length();
                    foundBegin = true;
                    foundFairBegin = true;
                } else {
                    algLine = algLine + 1;
                }
            }
        }
        if (!foundBegin) {
            PcalDebug.reportError("Beginning of algorithm string " + PcalParams.BeginAlg + " not found.");
            return null;
        } else {
            Path path = Paths.get(".env/banner.txt");
            String banner = "";
            try {
                banner = FileUtils.readFileToString(path.toFile(), StandardCharsets.UTF_8);
            } catch (IOException e) {
                PcalDebug.reportError("Could not read banner file: " + path.toAbsolutePath());
            }
            PcalDebug.reportInfo(banner);
            PcalDebug.reportInfo(" ✅ Beginning of algorithm string " + algLine + ":" + algCol + " found.");
        }

        /*
         * Set the algColumn and algLine fields of the mapping object.
         */
        mapping.algColumn = algCol;
        mapping.algLine = algLine;

        if (translationLine == -1) {
            /****************************************************************
             * Insert BEGIN/END TRANSLATION comments immediately after the   *
             * end of the comment that contains the beginning of the         *
             * algorithm.  Set translationLine to the (Java) line number of  *
             * the BEGIN TRANSLATION.                                        *
             ****************************************************************/

            // Set ecLine, ecCol to the position immediately after the
            // *) that closes the current comment.
            int depth = 1;
            int ecLine = algLine;
            int ecCol = algCol;
            boolean notFound = true;
            while (notFound && ecLine < untabInputVec.size()) {
                char[] line = ((String) untabInputVec.elementAt(ecLine)).toCharArray();

                // check current line 
                while (notFound && ecCol < line.length - 1) {
                    char ch = line[ecCol];
                    char ch2 = line[ecCol + 1];
                    if (ch == '(' && ch2 == '*') {
                        // left comment delimiter
                        depth++;
                        ecCol = ecCol + 2;
                    } else if (ch == '*' && ch2 == ')') {
                        // right comment delimiter
                        depth--;
                        ecCol = ecCol + 2;
                        if (depth == 0) {
                            notFound = false;
                        }
                    } else {
                        // not an interesting character
                        ecCol++;
                    }
                }

                // if not found, go to next line
                if (notFound) {
                    ecLine++;
                    ecCol = 0;
                }
            }

            if (notFound) {
                PcalDebug.reportError("Algorithm not in properly terminated comment");
                return null;
            }

            // Report an error  if there's something else on the line that doesn't begin with "\*".  This is probably
            String endStuff = ((String) untabInputVec.elementAt(ecLine)).substring(ecCol).trim();

            if (!endStuff.equals("") && !endStuff.startsWith("\\*")) {
                PcalDebug.reportError("Text on same line following `*)' that ends the \n   comment containing the algorithm.");
                return null;
            }
            ;

            tlaOutput.add((ecLine + 1), (PCAL_TRANSLATION_COMMENT_LINE_PREFIX + " " + String.format(Validator.CHECKSUM_TEMPLATE, "ffffffff", "ffffffff")));
            untabInputVec.insertElementAt(PCAL_TRANSLATION_COMMENT_LINE_PREFIX, (ecLine + 1));
            tlaOutput.add((ecLine + 2), (TLA_TRANSLATION_COMMENT_LINE_PREFIX + " "));
            untabInputVec.insertElementAt(TLA_TRANSLATION_COMMENT_LINE_PREFIX, (ecLine + 2));

            translationLine = ecLine + 1;
        } else {
            // Check if the existing TLA+ translation has been modified by the user and
            // raise a warning (via cb) if translation should be cancelled to not
            // lose/overwrite the user changes.
            final Matcher m = Validator.CHECKSUM_PATTERN.matcher(tlaOutput.get(translationLine));
            if (m.find() && m.group(Validator.TLA_CHECKSUM) != null) {
                final String checksumTLATranslation = Validator.checksum(new Vector<>(specificationText.subList((translationLine + 1), endTranslationLine)));
                if (!m.group(Validator.TLA_CHECKSUM).equals(checksumTLATranslation) && cb.shouldCancel()) {
                    return null;
                }
            }
        }

        /*
         * Set the mappings start line.
         */
        mapping.tlaStartLine = translationLine + 1;

        /*********************************************************************
         * Added by LL on 18 Feb 2006 to fix bugs related to handling of      *
         * comments.                                                          *
         *                                                                    *
         * Remove all comments from the algorithm in untabInputVec,           *
         * replacing (* *) comments by spaces to keep the algorithm tokens    *
         * in the same positions for error reporting.                         *
         *********************************************************************/
        try {
            ParseAlgorithm.uncomment(untabInputVec, algLine, algCol);
        } catch (ParseAlgorithmException e) {
            PcalDebug.reportError(e);
//            return exitWithStatus(STATUS_EXIT_WITH_ERRORS);
            return null; // added for testing
        }
        // } // end else of if (PcalParams.fromPcalFile) -- i.e., end processing
        // of .tla input file.

        /*********************************************************************
         * Set reader to a PcalCharReader for the input file (with tabs and   *
         * the previous translation removed), starting right after the        *
         * PcalParams.BeginAlg string.                                        *
         *********************************************************************/
        PcalCharReader erlangReader = new PcalCharReader(untabInputVec, algLine, algCol, tlaOutput.size(), 0, true);
        PcalCharReader reader = new PcalCharReader(untabInputVec, algLine, algCol, tlaOutput.size(), 0, true);

        /*********************************************************************
         * Set ast to the AST node representing the entire algorithm.         *
         *********************************************************************/
        AST ast;
        AST erlangAst;
        Pair<AST, Vector> astAndMacros;
        try {
            ast = ParseAlgorithm.getAlgorithm(reader, foundFairBegin, false);
            astAndMacros = ParseAlgorithm.getAlgorithm(erlangReader, foundFairBegin, true, true);
            assert astAndMacros != null;
            assert ast != null;
            erlangAst = astAndMacros.getFirst();
        } catch (ParseAlgorithmException e) {
            PcalDebug.reportError(e);
            return null; // added for testing
        }
        PcalDebug.reportSuccess("Parsing completed.");
        if (PcalParams.WriteASTFlag) {
            WriteAST(ast);
            return null; // added for testing
        }
        PCalTLAGenerator pcalTLAGenerator = new PCalTLAGenerator(ast);
        try {
            pcalTLAGenerator.removeNameConflicts();
        } catch (RemoveNameConflictsException e1) {
            PcalDebug.reportError(e1);
            return null;
        }
        Vector<String> translationTLA;
        PcalSymTab st;
        if (PcalParams.tlcTranslation()) {
            try {
                translationTLA = TLCTranslate(ast);
            } catch (TLCTranslationException e) {
                PcalDebug.reportError(e);
                return null; // added for testing
            }
        } else {
            try {
                translationTLA = pcalTLAGenerator.translate();
            } catch (RemoveNameConflictsException e) {
                PcalDebug.reportError(e);
                return null; // added for testing
            }

        }

        final Matcher m = Validator.CHECKSUM_PATTERN.matcher(tlaOutput.get(mapping.tlaStartLine - 1));
        Generate g;
        if (m.find()) {
            if (m.group(Validator.TLA_CHECKSUM) != null) {
                tlaOutput.set(mapping.tlaStartLine - 1, new StringBuilder(tlaOutput.get(mapping.tlaStartLine - 1)).replace(m.start(Validator.TLA_CHECKSUM), m.end(Validator.TLA_CHECKSUM), Validator.checksum(translationTLA)).toString());
            }
            if (m.group(Validator.PCAL_CHECKSUM) != null) {
                tlaOutput.set(mapping.tlaStartLine - 1, new StringBuilder(tlaOutput.get(mapping.tlaStartLine - 1)).replace(m.start(Validator.PCAL_CHECKSUM), m.end(Validator.PCAL_CHECKSUM),
                        // --fair algorithm is not reflected in the AST, which is why we prepend it here.
                        Validator.checksum(foundFairBegin ? Validator.FAIR : "" + ast)).toString());
            }
        } else if ((g = cb.shouldGenerate()) != Generate.NOT_NOW) {
            if (g == Generate.DO_IT) {
                tlaOutput.set(mapping.tlaStartLine - 1, tlaOutput.get(mapping.tlaStartLine - 1) + " " + String.format(Validator.CHECKSUM_TEMPLATE, Validator.checksum(foundFairBegin ? Validator.FAIR : "" + ast.toString()), Validator.checksum(translationTLA)));
            } else {
                tlaOutput.set(mapping.tlaStartLine - 1, tlaOutput.get(mapping.tlaStartLine - 1) + " " + Validator.CHECKSUM_TEMPLATE_IGNORE);
            }
        }
        int i = 0;
        while (i < translationTLA.size()) {
            tlaOutput.add((i + translationLine + 1), translationTLA.elementAt(i));
            i = i + 1;
        }

        PcalDebug.reportSuccess("PlusCal translation completed.");

        PcalErlangGenResult result = null;
        if (!disableErlangTranslation && PcalParams.TranslateErlang) {
            PcalDebug.reportSuccess("Started Erlang translation.");
            st = pcalTLAGenerator.GetSymTab();

            if (erlangAst == null || st == null) {
                PcalDebug.reportError("Exploded AST or SymTab is not initialized, aborting Erlang translation");
                return null;
            } else {
                PcalErlangGenerator pcalErlangGenerator = new PcalErlangGenerator(st, erlangAst, astAndMacros.getSecond(), skipStartFunctions);
                try {
                    result = pcalErlangGenerator.translate();
                } catch (PcalErlangGenException e) {
                    PcalDebug.reportError(e);
                    return null;
                } catch (NoSuchFieldException e) {
                    throw new RuntimeException(e);
                }
            }
            PcalDebug.reportSuccess("Erlang translation completed.");
        } else {
            PcalDebug.reportInfo("Skipping Erlang translation.");
        }
        return new TranslationResult(tlaOutput, result);
    }

    /**
     * If run in the system mode, exits the program, in tool mode returns the status
     *
     * @param status
     */
    private static int exitWithStatus(int status) {
        if (ToolIO.getMode() == ToolIO.SYSTEM) {
            // return exit status in system mode
            System.exit(status);
        }

        // just exit the function in tool mode
        return status;
    }

    /********************** Writing the AST ************************************/
    private static boolean WriteAST(AST ast) {
        Vector<String> astFile = new Vector<String>();
        astFile.addElement("------ MODULE AST -------");
        astFile.addElement("EXTENDS TLC");
        astFile.addElement("fairness == \"" + PcalParams.FairnessOption + "\"");
        astFile.addElement(" ");
        astFile.addElement("ast == ");
        astFile.addElement(ast.toString());
        astFile.addElement("==========================");
        try {
            WriteStringVectorToFile(astFile, "AST.tla");
        } catch (StringVectorToFileException e) {
            PcalDebug.reportError(e);
            return false;
        }
        PcalDebug.reportInfo("Wrote file AST.tla.");
        return true;
    }

    /************************* THE TLC TRANSLATION *****************************/

    private static Vector<String> TLCTranslate(AST ast) throws TLCTranslationException
    /***********************************************************************
     * The result is a translation of the algorithm represented by ast      *
     * obtained by using TLC to execute the definition of Translation(ast)  *
     * in the TLA+ module PlusCal.  It equals a vector with a single        *
     * element, which is the entire translation as a single string.         *
     *                                                                      *
     * This method relies on a bug in TLC's pretty-print routine that       *
     * causes it not to work properly on the output produced by the TLA     *
     * spec.  Instead of prettyprinting the output as                       *
     *                                                                      *
     *   << "VARIABLES ...",                                                *
     *      "vars == ... ",                                                 *
     *      ...                                                             *
     *   >>                                                                 *
     *                                                                      *
     * it prints the entire translation on a single line as                 *
     *                                                                      *
     *   << "VARIABLES ...", "vars == ... ", ... >>                         *
     *                                                                      *
     * This allows the method to find the entire translation as the single  *
     * line that begins with "<<".  If this TLC bug is fixed, then the      *
     * TLCTranslate method will have to be modified to read the spec as a   *
     * sequence of lines.  This will probably require the TLA module that   *
     * translates the spec to print a special marker line to indicate the   *
     * end of the translation.                                              *
     ***********************************************************************/
    {
        /*********************************************************************
         * Write the file AST.tla that contains                               *
         *********************************************************************/
        WriteAST(ast);

        /*********************************************************************
         * For the -spec (rather than -myspec) option, copy the               *
         * specification's .tla and .cfg files PlusCal.tla to current         *
         * directory.                                                         *
         *********************************************************************/
        if (PcalParams.SpecOption || PcalParams.Spec2Option) {
            try {
                Vector<String> parseFile = PcalResourceFileReader.ResourceFileToStringVector(PcalParams.SpecFile + TLAConstants.Files.TLA_EXTENSION);

                WriteStringVectorToFile(parseFile, PcalParams.SpecFile + TLAConstants.Files.TLA_EXTENSION);
                parseFile = PcalResourceFileReader.ResourceFileToStringVector(PcalParams.SpecFile + TLAConstants.Files.CONFIG_EXTENSION);
                WriteStringVectorToFile(parseFile, PcalParams.SpecFile + TLAConstants.Files.CONFIG_EXTENSION);

                PcalDebug.reportInfo("Wrote files " + PcalParams.SpecFile + TLAConstants.Files.TLA_EXTENSION + " and " + PcalParams.SpecFile + TLAConstants.Files.CONFIG_EXTENSION + ".");
            } catch (UnrecoverableException e) {
                throw new TLCTranslationException(e.getMessage());
            }

        }
        ;
        /*********************************************************************
         * Run TLC on the specification file and set tlcOut to TLC's output.  *
         *********************************************************************/
        String javaInvocation;
        if (PcalParams.SpecOption || PcalParams.MyspecOption) {
            // Modified on 29 May 2010 by LL so tlc2 is run in
            // all cases.
            PcalDebug.reportInfo("Running TLC2.");
            javaInvocation = "java -Xss1m tlc2.TLC ";
        } else {
            PcalDebug.reportInfo("Running TLC2.");
            javaInvocation = "java -Xss1m tlc2.TLC ";
        }
        ;
        String tlcOut = "      ";
        Runtime rt = Runtime.getRuntime();
        try {
            // Modified on 29 May 2010 by LL to replace getErrorStream() with 
            // getInputStream(), which by Java logic gets standard out.  (And no,
            // getErrorStream() did not get standard non-error.)  Apparently,
            // TLC has been changed to put its output on stdout.
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(rt.exec(javaInvocation + PcalParams.SpecFile).getInputStream()));
            while (tlcOut.indexOf("<<") == -1) {
                tlcOut = bufferedReader.readLine();
            }
            ;
            bufferedReader.close();
        } catch (Exception e) {
            throw new TLCTranslationException("Error reading output of TLC");
        }
        ;

        /*********************************************************************
         * Test if the translation failed and reported an error message,      *
         * bracketed by "@Error@" and "@EndError@" strings.  If it did,       *
         * report the error and halt.  If not, set tlcOut to the value of     *
         * Translation(ast) with the outermost "<<" and ">>" removed.         *
         *********************************************************************/
        if (tlcOut.indexOf("@Error@") != -1) {
            throw new TLCTranslationException("TLC's translation of the parsed algorithm failed with\n  Error: " + tlcOut.substring(tlcOut.indexOf("@Error@") + 7, tlcOut.indexOf("@EndError@")));
        }
        ;
        tlcOut = tlcOut.substring(2, tlcOut.lastIndexOf(">>")) + "  ";
        PcalDebug.reportInfo("Read TLC output.");

        /*********************************************************************
         * Set transl to the string obtained by converting tlcOut, which is   *
         * a comma-separated sequence of strings, to the single string that   *
         * they represent.  See PlusCal.tla for an explanation of the         *
         * encoding of TLA+ statements as sequences of strings.               *
         *********************************************************************/
        int i = 0;
        String transl = "";
        while (i < tlcOut.length()) {
            if (tlcOut.charAt(i) == '"') {
                i = i + 1;
                if ((tlcOut.charAt(i) == '\\') && (tlcOut.charAt(i + 1) == '"'))
                /*******************************************************
                 * This is a quoted string.                             *
                 *******************************************************/ {
                    if (tlcOut.charAt(i + 2) != '"') {
                        throw new TLCTranslationException("I'm confused");

                    }
                    ;
                    i = i + 3;
                    while (tlcOut.charAt(i) != '"') {
                        i = i + 1;
                    }
                    i = i + 1;
                    transl = transl + "\"";
                    while (tlcOut.charAt(i) != '"') // "
                    {
                        if (tlcOut.charAt(i) == '\\') {
                            /***********************************************
                             * Get special character.                       *
                             ***********************************************/
                            transl = transl + tlcOut.substring(i, i + 2);
                            i = i + 2;
                        } else {
                            transl = transl + tlcOut.substring(i, i + 1);
                            i = i + 1;
                        }
                        ;
                    }
                    ;
                    i = i + 8;
                    transl = transl + "\"";
                } else {
                    while (tlcOut.charAt(i) != '"') {
                        if ((tlcOut.charAt(i) == '\\') && (tlcOut.charAt(i + 1) == '\\')) {
                            i = i + 1;
                        }
                        ;
                        transl = transl + tlcOut.substring(i, i + 1);
                        i = i + 1;
                    }
                    ;
                    i = i + 1;
                }
                ;
            } // END if (tlcOut.charAt(i) == '"')
            else if (tlcOut.charAt(i) == ',') {
                i = i + 1;
            } else {
                if (tlcOut.charAt(i) != ' ') {
                    throw new TLCTranslationException("Expected space but found `" + tlcOut.charAt(i) + "'");
                }
                ;
                transl = transl + tlcOut.substring(i, i + 1);
                i = i + 1;
            }
            ;
        }
        ; // END while
        /* ******************************************************************
         * Wrap the translated string into approximately 80 character lines *
         *******************************************************************/
        transl = WrapString(transl, 78);
        Vector<String> result = new Vector<String>();
        result.addElement(transl);
        return result;
    }

    /***************** METHODS FOR READING AND WRITING FILES *****************/

    private static void WriteStringVectorToFile(final List<String> inputVec, String fileName) throws StringVectorToFileException
    /***********************************************************************
     * Writes the List of strings inputVec to file named fileName, with     *
     * each element of inputVec written on a new line.                      *
     ***********************************************************************/
    {
        try {
            String content = String.join("\n", inputVec);
            Path path = Paths.get(fileName);
            Files.createDirectories(path.getParent());
            FileUtils.writeStringToFile(path.toFile(), content, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new StringVectorToFileException("Could not write file " + fileName);
        }
    }

    private static List<String> fileToStringVector(String fileName) throws FileToStringVectorException
    /***********************************************************************
     * Reads file fileName into a StringVector, a vector in which each      *
     * element is a line of the file.                                       *
     ***********************************************************************/
    {
        final List<String> inputVec = new ArrayList<>(100);
        // TODO use Apache Commons for this
        try (final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)))) {
            String nextLine = bufferedReader.readLine();
            while (nextLine != null) {
                inputVec.add(nextLine);
                nextLine = bufferedReader.readLine();
            }
        } catch (FileNotFoundException e) {
            /**************************************************************
             * Input file could not be found.                              *
             **************************************************************/
            throw new FileToStringVectorException("Input file " + fileName + " not found.");
        } catch (IOException e) {
            /*********************************************************
             * Error while reading input file.                        *
             *********************************************************/
            throw new FileToStringVectorException("Error reading file " + fileName + ".");
        }

        return inputVec;
    }

    /********************* PROCESSING THE COMMAND LINE ***********************/

    /**
     * Processes the command line arguments
     * <p>
     * This method changes values of public static variables of the {@link PcalParams}
     * <p>
     * SZ: This will cause problems, due to the fact that the PCalParams are static.
     * Any initialization should overwrite the previous, which is currently NOT the case
     * Should be re-factored to non-static access to the properties
     *
     * @return status of processing.
     * the status {@link trans#STATUS_OK} indicates that no errors has been detected.
     * the status {@link trans#STATUS_EXIT_WITHOUT_ERROR} indicates that no errors has been found but translation
     * should not be started (e.G -help call)
     * the status {@link trans#STATUS_EXIT_WITH_ERRORS} indicates errors
     * <p>
     * Change made on 9 December 2009 for pcal-file input.  This procedure is
     * called a second time if there is pcal-file input with an options statement.
     * It will be the second call iff {@link PcalParams#optionsInFile} equals true.
     * The second call should have a dummy extra argument in place of the
     * command-line's file-name argument.   When pcal files were eliminated, this
     * kludgy mechanism was kept and used to indicate if the method is being called
     * for options specified inside the module.
     */
    static int parseAndProcessArguments(String[] args) {

        /** *******************************************************************
         *<pre>
         * Get the command-line arguments and set the appropriate parameters.  *
         * The following command line arguments are handled.  Only the ones    *
         * marked with ** besides them can be specified in the module file's   *
         * options statement.  The "-" can be omitted when the option is in    *
         * the module file's options statement.                                *
         *                                                                     *
         *   -help  : Type a help file instead of running the program.         *
         *                                                                     *
         *** -spec name : Uses TLC and the TLA+ specification name.tla to do   *
         *                the translation.  The files name.tla and name.cfg    *
         *                are copied from the java/ directory to the current   *
         *                directory; the file AST.tla that defines `fairness'  *
         *                to equal the fairness option and `ast' to equal      *
         *                the the AST data structure representing the          *
         *                algorithm is written to the current directory; and   *
         *                TLC is run on name.tla to produce the translation.   *
         *                                                                     *
         *** -myspec name : Like -spec, except it finds the files names.tla    *
         *                  and names.cfg in the current directory, instead    *
         *                  of writing them there.                             *
         *                                                                     *
         *   -spec2   name                                                     *
         *   -myspec2 name : Like -spec and -myspec, except they use TLC2      *
         *                   instead of TLC (aka TLC1).                        *
         *                                                                     *
         *   -writeAST : Writes the AST file as in the -spec option, but does  *
         *               not perform the translation.                          *
         *                                                                     *
         *   -debug : Run in debugging mode, whatever that means.  For the     *
         *            parser, it just means that the toString() methods        *
         *            output the line and column number along with the         *
         *            node.                                                    *
         *                                                                     *
         *   -unixEOL : Forces the use of Unix end-of-line convention,         *
         *              regardless of the system's default.  Without this,     *
         *              when run on Windows, the files it writes seem to have  *
         *              a "^M" at the end of every line when viewed with       *
         *              Emacs.                                                 *
         *                                                                     *
         *** -wf : Conjoin to formula Spec weak fairness of each process's     *
         *         next-state action                                           *
         *                                                                     *
         *** -sf : Conjoin to formula Spec strong fairness of each process's   *
         *         next-state action                                           *
         *                                                                     *
         *** -wfNext : Conjoin to formula Spec weak fairness of the entire     *
         *             next-state action                                       *
         *                                                                     *
         *** -nof : Conjoin no fairness formula to Spec.  This is the default, *
         *          except when the -termination option is chosen.             *
         *                                                                     *
         *** -termination : Add to the .cfg file the command                   *
         *                     PROPERTY Termination                            *
         *                                                                     *
         *   -nocfg : Suppress writing of the .cfg file.                       *
         *                                                                     *
         *                                                                     *
         *** -noDoneDisjunct : Suppress the disjunct of the next-state         *
         *                     relation that describes stuttering steps taken  *
         *                     when the algorithm has halted.                  *
         *                                                                     *
         *** -label : Tells the translator to add missing labels.  This is     *
         *            the default only for a uniprocess algorithm in which     *
         *            the user has typed no labels.                            *
         *                                                                     *
         *   -reportLabels : True if the translator should print the names     *
         *                   and locations of all labels it adds.  Like        *
         *                   -label, it tells the translator to add missing    *
         *                   labels.                                           *
         *                                                                     *
         *** -labelRoot name : If the translator adds missing labels, it       *
         *                     names them name1, name2, etc.  Default value    *
         *                     is "Lbl_".                                      *
         *                                                                     *
         *  THE FOLLOWING OPTIONS ADDED IN VERSION 1.4                         *
         *                                                                     *
         *** -lineWidth : The translation tries to perform the translation so  *
         *                lines have this maximum width.  (It will often       *
         *                fail.)  Default is 78, minimum value is 60.          *
         *                                                                     *
         *** -version : The version of PlusCal for which this algorithm is     *
         *              written.  If the language is ever changed to make      *
         *              algorithms written for earlier versions no longer      *
         *              legal, then the translator should do the appropriate   *
         *              thing when the earlier version number is specified.    *
         *              				                                       *                
         *** -erlang : Specifies whether an Erlang program should be           *
         *             generated.                                              *
         *                                                                     *
         *** -genMain : Specifies whether the translator should generate       *
         *              a main function in the Erlang translation.             *
         *              This flag is only useful in conjunction                *
         *              with "-erlang" and is meant as a demonstration         *
         *              of how to start the generated processes.               *
         *              If control over the order of how the translated        *
         *              processes are started is desired, then it is better    *
         *              not to set this flag.                                  *
         *                                                                     *
         *                                                                     *
         *</pre>
         ********************************************************************* */
        boolean inFile = PcalParams.optionsInFile;
        boolean notInFile = !inFile;
        // Just convenient abbreviations
        boolean firstFairness = inFile;
        // Used to allow a fairness property specified by a command-line
        // option to be overridden by one in the pcal-file's options statement.
        // It is set false when the first fairness property is set from
        // the options statement.
        boolean explicitNof = false;
        // Set true when the "nof" fairness option is set by an explicit
        // user request, rather than by default. It was added to fix
        // a bug in -termination introduced in version 1.4 by having
        // the options statement in the file. I think option processing
        // can be simplified to eliminate this, but it's easier to add
        // this kludge.
        int nextArg = 0;
        /******************************************************************
         * The number of the argument being processed.                     *
         ******************************************************************/
        int maxArg = args.length - 1;
        /******************************************************************
         * The number of option arguments.  (For processing command-line   *
         * arguments, the last element of args is the input-file name.)    *
         ******************************************************************/
        if (maxArg < 0) {
            return CommandLineError("No arguments specified");
        }

        if (notInFile && (args[maxArg].length() != 0) && (args[maxArg].charAt(0) == '-'))
        /******************************************************************
         * If the last argument begins with "-", then no file has been     *
         * specified.  This should mean that the user has typed "-help",   *
         * but it could be a mistake.  But let's just assume she typed     *
         * "-help", since she either wants or needs help.                  *
         ******************************************************************/ {
            if (OutputHelpMessage()) {
                return STATUS_EXIT_WITHOUT_ERROR;

            } else {
                return STATUS_EXIT_WITH_ERRORS;
            }
        }

        while (nextArg < maxArg)
        /*******************************************************************
         * Process all the arguments, except for the input-file name.       *
         *******************************************************************/ {
            String option = args[nextArg];
            if (option.isBlank() || option.isEmpty()) {
                nextArg++;
                continue;
            } else if (notInFile && option.equals("-help")) {
                if (OutputHelpMessage()) {
                    return STATUS_EXIT_WITHOUT_ERROR;
                } else {
                    return STATUS_EXIT_WITH_ERRORS;
                }
            } else if (notInFile && option.equals("-writeAST")) {
                PcalParams.WriteASTFlag = true;
                if (CheckForConflictingSpecOptions()) {
                    return STATUS_EXIT_WITH_ERRORS;
                }
            } else if (option.equals("-spec") || (inFile && option.equals("spec"))) {
                PcalParams.SpecOption = true;
                if (CheckForConflictingSpecOptions()) {
                    return STATUS_EXIT_WITH_ERRORS;
                }
                nextArg = nextArg + 1;
                if (nextArg == maxArg) {
                    return CommandLineError("Specification name must follow `-spec' option");
                }
                PcalParams.SpecFile = args[nextArg];
            } else if (option.equals("-myspec") || (inFile && option.equals("myspec"))) {
                PcalParams.MyspecOption = true;
                if (CheckForConflictingSpecOptions()) {
                    return STATUS_EXIT_WITH_ERRORS;
                }
                nextArg = nextArg + 1;
                if (nextArg == maxArg) {
                    return CommandLineError("Specification name must follow `-myspec' option");
                }
                PcalParams.SpecFile = args[nextArg];
            } else if (notInFile && option.equals("-spec2")) {
                PcalParams.Spec2Option = true;
                if (CheckForConflictingSpecOptions()) {
                    return STATUS_EXIT_WITH_ERRORS;
                }
                ;
                nextArg = nextArg + 1;
                if (nextArg == maxArg) {
                    return CommandLineError("Specification name must follow `-spec' option");
                }
                PcalParams.SpecFile = args[nextArg];
            } else if (notInFile && option.equals("-myspec2")) {
                PcalParams.Myspec2Option = true;
                if (CheckForConflictingSpecOptions()) {
                    return STATUS_EXIT_WITH_ERRORS;
                }
                ;
                nextArg = nextArg + 1;
                if (nextArg == maxArg) {
                    return CommandLineError("Specification name must follow `-myspec' option");
                }
                PcalParams.SpecFile = args[nextArg];
            } else if (notInFile && option.equals("-debug")) {
                PcalParams.Debug = true;
            } else if (notInFile && option.equals("-unixEOL")) {
                System.setProperty("line.separator", "\n");
            } else if (option.equals("-termination") || (inFile && option.equals("termination"))) {
                PcalParams.CheckTermination = true;
            } else if (option.equals("-noold")) {
                PcalParams.NoOld = true;
            } else if (option.equals("-nocfg")) {
                PcalParams.Nocfg = true;
            } else if (option.equals("-noDoneDisjunct") || (inFile && option.equals("noDoneDisjunct"))) {
                PcalParams.NoDoneDisjunct = true;
            } else if (option.equals("-wf") || (inFile && option.equals("wf"))) {
                if (firstFairness) {
                    PcalParams.FairnessOption = "";
                    firstFairness = false;
                }
                if (!PcalParams.FairnessOption.equals("")) {
                    return CommandLineError("Can only have one of -wf, -sf, -wfNext, " + "and -nof options");
                }
                PcalParams.FairnessOption = "wf";
            } else if (option.equals("-sf") || (inFile && option.equals("sf"))) {
                if (firstFairness) {
                    PcalParams.FairnessOption = "";
                    firstFairness = false;
                }
                if (!PcalParams.FairnessOption.equals("")) {
                    return CommandLineError("Can only have one of -wf, -sf, -wfNext, " + "and -nof options");
                }
                PcalParams.FairnessOption = "sf";
            } else if (option.equals("-wfNext") || (inFile && option.equals("wfNext"))) {
                if (firstFairness) {
                    PcalParams.FairnessOption = "";
                    firstFairness = false;
                }
                if (!PcalParams.FairnessOption.equals("")) {
                    return CommandLineError("Can only have one of -wf, -sf, -wfNext, " + "and -nof options");
                }
                PcalParams.FairnessOption = "wfNext";
            } else if (option.equals("-nof") || (inFile && option.equals("nof"))) {
                if (firstFairness) {
                    PcalParams.FairnessOption = "";
                    firstFairness = false;
                }
                if (!PcalParams.FairnessOption.equals("")) {
                    return CommandLineError("Can only have one of -wf, -sf, -wfNext, " + "and -nof options");
                }
                PcalParams.FairnessOption = "nof";
                explicitNof = true;
            } else if (option.equals("-label") || (inFile && option.equals("label"))) {
                PcalParams.LabelFlag = true;
            } else if (option.equals("-erlang")) {
                PcalParams.TranslateErlang = true;
            } else if (option.equals("-genMain")) {
                PcalParams.GenErlangMainFunction = true;
            } else if (notInFile && option.equals("-reportLabels")) {
                PcalParams.ReportLabelsFlag = true;
                PcalParams.LabelFlag = true;
            } else if (option.equals("-labelRoot") || (inFile && option.equals("labelRoot"))) {
                nextArg = nextArg + 1;
                if (nextArg == maxArg) {
                    return CommandLineError("Label root must follow `-labelRoot' option");
                }
                PcalParams.LabelRoot = args[nextArg];
            }
            // else if (option.equals("-readOnly") || (pcal && option.equals("readOnly"))) {
            // PcalParams.readOnly = true;
            // }
            // else if (option.equals("-writable") || (pcal && option.equals("writable"))) {
            // PcalParams.readOnly = false;
            // }
            else if (option.equals("-version") || (inFile && option.equals("version"))) {
                nextArg = nextArg + 1;
                if (nextArg == maxArg) {
                    return CommandLineError("Version number must follow `-version' option");
                }
                if (!PcalParams.ProcessVersion(args[nextArg])) {
                    return CommandLineError("Bad version number");
                }

            } else if (option.equals("-lineWidth")) {
                nextArg = nextArg + 1;
                try {
                    if (nextArg == maxArg) {
                        throw new NumberFormatException();
                    }
                    int a = Integer.parseInt(args[nextArg]);
                    if (a < 60) {
                        throw new NumberFormatException();
                    }
                    PcalTLAGen.wrapColumn = a;
                    PcalTLAGen.ssWrapColumn = a - 33;
                } catch (Exception e) {
                    return CommandLineError("Integer value at least 60 must follow `-lineWidth' option");
                }

            } else {
                if (notInFile) {
                    return CommandLineError("Unknown command-line option: " + option);
                } else {
                    return CommandLineError("Unknown or illegal option in options statement: " + option);
                }
            }
            ;
            nextArg = nextArg + 1;
        } // END while (nextArg < maxArg)

        if (nextArg > maxArg)
        /******************************************************************
         * The last option took an argument that was the last              *
         * command-line argument.                                          *
         ******************************************************************/ {
            return CommandLineError("No input file specified");
        }

        // SZ 02.16.2009: since this is a modification of the parameters, moved
        // to the parameter handling method
        if (PcalParams.FairnessOption.equals("-nof")) {
            PcalParams.FairnessOption = "";
        }
        if (PcalParams.CheckTermination && PcalParams.FairnessOption.equals("") && !explicitNof) {
            PcalParams.FairnessOption = "wf";

        }

        /********************************************************************
         * If we are processing the command-line arguments, we need to get  *
         * the input-file name.  Otherwise, we're done.                     *     
         *******************************************************************/
        if (inFile) {
            return STATUS_OK;
        }

        /********************************************************************
         * Set PcalParams.TLAInputFile to the last argument, removing a      *
         * "tla" extension if it has one.                                    *
         ********************************************************************/
        /*
        int dotIndex = args[maxArg].lastIndexOf(".") ;
        if (dotIndex == -1) 
        { 
          PcalParams.TLAInputFile = args[maxArg]; 
        } 
        else if (args[maxArg].substring(dotIndex).equals(TLAConstants.FILE_TLA_EXTENSION))
        { 
          PcalParams.TLAInputFile = args[maxArg].substring(0, dotIndex); 
        }
        else 
        {  
          return CommandLineError("Input file has extension other than tla"); 
        }
        */

        // SZ 02.16.2009: check for correct file extension (ignoring case)
        // and file existence. also handles dots in the pathname
        File file = new File(args[maxArg]);
        boolean hasExtension = false;
        if (file.getName().lastIndexOf(".") == -1) {
            // no extension
            PcalParams.TLAInputFile = file.getPath();
        } else {
            // extension present
            if (file.getName().toLowerCase().endsWith(TLAConstants.Files.TLA_EXTENSION)) {
                hasExtension = true;
            }
            // Aborted version 1.31 code
            // else if (file.getName().toLowerCase().endsWith(".pcal")){
            // hasExtension = true;
            // PcalParams.fromPcalFile = true;
            // }
            else {
                return CommandLineError("Input file has extension other than " /* pcal or */ + "tla");
            }
        }
        if (hasExtension) {
            // cut the extension
            PcalParams.TLAInputFile = file.getPath().substring(0, file.getPath().lastIndexOf("."));
            if (!file.exists()) {
                return CommandLineError("Input file " + file.getPath() + " does not exist.");
            }
        } else {
            // aborted version 1.31 code
            // file = new File(PcalParams.TLAInputFile + ".pcal");
            // if (file.exists())
            // {
            // PcalParams.fromPcalFile = true;
            // } else
            // {
            file = new File(PcalParams.TLAInputFile + TLAConstants.Files.TLA_EXTENSION);
            if (!file.exists()) {
                return CommandLineError("Input file " + PcalParams.TLAInputFile + ".pcal and " + file.getPath() + ".tla not found");
            }
            // }
        }
        // file = new File(PcalParams.TLAInputFile + (PcalParams.fromPcalFile?".pcal":TLAConstants.FILE_TLA_EXTENSION));
        // if (!file.exists())
        // {
        // return CommandLineError("Input file " + file.getPath() + " not found");
        // }

        return STATUS_OK;
    }

    /**
     * Prints out the help message
     *
     * @return status if it has been successfully printed
     */
    private static boolean OutputHelpMessage() {
        Vector<String> helpVec = null;
        try {
            helpVec = PcalResourceFileReader.ResourceFileToStringVector("help.txt");
        } catch (PcalResourceFileReaderException e) {
            PcalDebug.reportError(e);
            return false;
        }
        int i = 0;
        while (i < helpVec.size()) {
            ToolIO.out.println((String) helpVec.elementAt(i));
            i = i + 1;
        }

        return true;
    }

    /**
     * Returns if the options are conflicting
     *
     * @return true if the provided options are conflicting, false otherwise
     */
    private static boolean CheckForConflictingSpecOptions() {
        if ((PcalParams.SpecOption ? 1 : 0) + (PcalParams.MyspecOption ? 1 : 0) + (PcalParams.Spec2Option ? 1 : 0) + (PcalParams.Myspec2Option ? 1 : 0) + (PcalParams.WriteASTFlag ? 1 : 0) > 1) {
            CommandLineError("\nCan have at most one of the options " + "-spec, -myspec, -spec2, -myspec2, writeAST");
            return true;
        }
        ;
        return false;
    }

    private static int CommandLineError(String msg)
    /*********************************************************************
     * Announce a command line error with the string indicating the       *
     * explanation and halt.                                              *
     *********************************************************************/
    {
        PcalDebug.reportError("Command-line error: " + msg + ".");
//        ToolIO.out.println("Command-line error: " + msg + ".");
//        ToolIO.out.println("Use -help option for more information.");
        return STATUS_EXIT_WITH_ERRORS;
    }

    static int findTokenPair(Vector<String> vec, int lineNum, String tok1, String tok2)
    /*********************************************************************
     * Returns the number of the first line at or after lineNum in the    *
     * vector of strings vec containing tok1 followed by 1 or more        *
     * spaces followed by tok2.  Returns -1 if such a line is not found.  *
     *********************************************************************/
    {
        int i = lineNum;
        while (i < vec.size()) {
            String line = vec.elementAt(i);
            int col = line.indexOf(tok1);
            int nextcol = col + tok1.length();
            if (col != -1) {
                while ((nextcol < line.length()) && (line.charAt(nextcol) == ' ')) {
                    nextcol = nextcol + 1;
                }
                ;
                if ((nextcol < line.length()) && (nextcol == line.indexOf(tok2))) {
                    return i;
                }
            }
            ;
            i = i + 1;
        }
        ;
        return -1;
    }

    /**************************  RemoveTabs  *********************************/

    public static Vector<String> removeTabs(List<String> input) {
        /********************************************************************
         * Returns a string vector obtained from the string vector vec by   *
         * replacing any evil tabs with the appropriate number of spaces,   *
         * where "appropriate" means adding from 1 to 8 spaces in order to  *
         * make the next character fall on a column with Java column        *
         * number (counting from 0) congruent to 0 mod 8.  This is what     *
         * Emacs does when told to remove tabs, which makes it good enough  *
         * for me.                                                          *
         ********************************************************************/
        final Vector<String> newVec = new Vector<>();
        for (final String oldLine : input) {
            String newLine = "";
            int next = 0;
            while (next < oldLine.length()) {
                if (oldLine.charAt(next) == '\t') {
                    int toAdd = 8 - (newLine.length() % 8);
                    while (toAdd > 0) {
                        newLine = newLine + " ";
                        toAdd = toAdd - 1;
                    }
                } else {
                    newLine = newLine + oldLine.substring(next, next + 1);
                }
                next = next + 1;
            }
            // The following line is a hack to eliminate a rare bug that caused 
            // the translation to loop forever if a line ended with a symbol
            // that is the prefix of a legal BUILT_IN token and that is not
            // a legal token but has a prefix that is a legal token--for
            // example "(+" and "::" (since ::= is a legal operator).
            // It was added by LL on 13 May 2020            
            newLine = newLine + " ";

            newVec.add(newLine);
        }

        return newVec;
    }

    /********************* STRING UTILITY FUNCTIONS ***********************/

    private static int NextSpace(String s, int cur)
    /********************************************************************
     * Returns the first space in s at or after col. If there is none,   *
     * return the index of the last character in s. Spaces in strings    *
     * are not treated as spaces. Assumes s[cur] is not in a string.     *
     ********************************************************************/
    {
        int i = cur;
        boolean inString = false;
        while ((i < s.length()) && ((s.charAt(i) != ' ') || inString)) {
            if ((s.charAt(i) == '"') && ((i == 0) || (s.charAt(i - 1) != '\\'))) inString = !inString;
            i = i + 1;
        }
        if (i == s.length()) return i - 1;
        else return i;
    }

    private static String WrapString(String inString, int col)
    /*********************************************************************
     * Returns the string inString with lines wrapped approximately at    *
     * col, taking care not to wrap in a string.                          *
     *********************************************************************/
    {
        int i = 0;
        int ccol = 1;
        StringBuffer sb = new StringBuffer();
        while (i < inString.length()) {
            if (inString.charAt(i) == ' ') // An initial space or a space
            {
                sb.append(' '); // that follows a space. It
                i = i + 1; // can always be appended to a line.
                ccol = ccol + 1;
            } else
            // Find next word, which starts at i.
            {
                int j = NextSpace(inString, i);
                if (ccol + (j - i + 1) > col) {
                    sb.append('\n');
                    ccol = 1;
                }
                while (i <= j) // If this overflows col, then the word
                {
                    sb.append(inString.charAt(i));
                    // is longer than col.
                    i = i + 1;
                    ccol = ccol + 1;
                }
            }
        }
        return sb.toString();
    }

}
