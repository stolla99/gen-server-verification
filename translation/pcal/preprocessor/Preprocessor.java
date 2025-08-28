package pcal.preprocessor;

import kotlin.Pair;
import pcal.*;
import pcal.ErlangProcessContext;
import pcal.PCalErlangConstants;
import pcal.exception.ParseAlgorithmException;
import pcal.exception.PcalErlangGenException;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

public class Preprocessor {
    static String basicAnnotation = "§[a-z]+\\([a-zA-Z_=, ]*\\)|§[a-z]+";

    public static String paragraphChar = "§";

    public static Pair<Integer, ArrayList<String>> calculateOffset(PcalCharReader charReader, PeekAtAlgTokenFunction peekAlgTokenFunction, AnnotationFunction annotationFunction) throws ParseAlgorithmException {
        int currentPeeks = 0;
        ArrayList<String> annotations = new ArrayList<>();
        Iterator<String> iterator = new Iterator<>() {
            private int linePeekCount = charReader.getLineNumber();

            @Override
            public boolean hasNext() {
                return annotationFunction.apply((String) charReader.vec.get(linePeekCount));
            }

            @Override
            public String next() {
                linePeekCount = linePeekCount + 1;
                return ((String) charReader.vec.get(linePeekCount - 1)).trim();
            }
        };
        while (iterator.hasNext()) {
            String next = iterator.next();
            StringBuilder annotation = new StringBuilder();
            while (!annotation.toString().equals(next.replaceAll(" ", ""))) {
                currentPeeks++;
                try {
                    annotation.append(peekAlgTokenFunction.apply(currentPeeks));
                } catch (ParseAlgorithmException ex) {
                    throw new RuntimeException("Error while parsing algorithm annotation collapse", ex);
                }
            }
            annotations.add(annotation.toString());

        }
        return new Pair<>(currentPeeks, annotations);
    }

    public static boolean isAnnotation(String token) {
        return Pattern.compile(basicAnnotation).matcher(token).find();
    }

    public static ArrayList<String> getLabels(List<String> annotation) {
        Pattern collapse = Pattern.compile("§collapse\\(.*\\)");
        Pattern pattern = Pattern.compile("(?:\\()(.*)(?:\\))");
        ArrayList<String> result = annotation.stream().collect(
                ArrayList::new,
                (list, s) -> {
                    if (collapse.matcher(s).find()) {
                        pattern.matcher(s).results().forEach(r -> {
                            list.addAll(Arrays.asList(r.group(1).split(",")));
                        });
                    }
                },
                ArrayList::addAll
        );
        return result;
    }

    public static AST.Process removeSkipLabels(AST.Process process, ArrayList<String> labels) throws NoSuchFieldException {
        AST.Process newProcess = process.deepCopy();
        ArrayList<Object> graveyard = new ArrayList<>();
        ArrayList<Vector> queue = new ArrayList<>();

        // First find statements to be removed
        queue.add(process.body);
        while (!queue.isEmpty()) {
            Vector current = queue.remove(0);
            for (Object astElement : current) {
                if (astElement instanceof AST.LabeledStmt labeledStmt) {
                    if (labels.contains(labeledStmt.label)) {
                        graveyard.add(astElement);
                    }
                    queue.add(labeledStmt.stmts);
                }
                exploreRest(queue, astElement);
            }
        }

        // Remove the labels from the process body
        queue.add(newProcess.body);
        while (!queue.isEmpty()) {
            Vector<Object> current = queue.remove(0);
            Vector<Object> toRemove = new Vector<>();
            for (Object astElement : current) {
                List<Object> matches = graveyard.stream().filter(obj -> astElement.toString().equals(obj.toString())).toList();
                if (!matches.isEmpty()) {
                    PcalDebug.reportInfo("      \uD83D\uDD34 removing " + astElement.toString().substring(0, 30).replace("\n", "") + "...");
                    toRemove.add(astElement);
                }
            }
            current.removeAll(toRemove);
            for (Object remainingAstElement: current) {
                if (remainingAstElement instanceof AST.LabeledStmt labeledStmt) {
                    queue.add(labeledStmt.stmts);
                }
                exploreRest(queue, remainingAstElement);
            }
        }
        return newProcess;
    }

    private static void exploreRest(ArrayList<Vector> queue, Object o) {
        if (o instanceof AST.While whileStmt) {
            queue.add(whileStmt.labDo);
        } else if (o instanceof AST.LabelIf labelIf) {
            queue.add(labelIf.labThen);
            queue.add(labelIf.labElse);
        } else if (o instanceof AST.LabelEither labelEither) {
            queue.add(labelEither.clauses);
        }
    }

    public static boolean doesProcessNeedsToHide(ArrayList<String> annotations) {
        return annotations.stream().anyMatch(a -> Pattern.compile("§hide|§hide\\(\\)").matcher(a).find());
    }

    public static boolean isItContext(String annotation) {
        return Pattern.compile("§context\\(from=[$a-zA-Z0-9_]+\\)").matcher(annotation).find();
    }

    public static String[] getFromTuple(String annotation) {
        String[] result = new String[1];
        Pattern fromPattern = Pattern.compile("§context\\(from=([$a-zA-Z0-9_]+)\\)");
        fromPattern.matcher(annotation).results().forEach(r -> {
            result[0] = r.group(1);
        });
        if (result[0] == null) {
            result[0] = "";
        }
        return result;
    }

    public static boolean isItPin(String annotation) {
        return Pattern.compile("§pin\\(name=[$a-zA-Z0-9_]+\\)" +
                "|§pin\\(name=[$a-zA-Z0-9_]+, *lazy=(true|false) *\\)" +
                "|§pin\\(name=[$a-zA-Z0-9_]+, *lazy=.* *convert=(.*) *" +
                "|§pin\\(name=[$a-zA-Z0-9_]+, *convert=(.*) *\\)" +
                "|§pin\\(name=[$a-zA-Z0-9_]+, *rec=(.*) *\\)").matcher(annotation).find();
    }

    public static String[] getNameLazyConvertRecTuple(String  annotation) {
        String[] tuple = new String[4];
        Pattern namePattern = Pattern.compile("§pin\\(name=([$a-zA-Z0-9_]+).*\\)");
        Pattern lazyPattern = Pattern.compile("§pin\\(.*lazy=(true|false).*\\)");
        Pattern convertPattern = Pattern.compile("§pin\\(.*convert=([a-zA-Z0-9_]+).*\\)");
        Pattern recPattern = Pattern.compile("§pin\\(.*rec=([a-zA-Z0-9_, ]+)\\)");
        namePattern.matcher(annotation).results().forEach(r -> {
            tuple[0] = r.group(1);
        });
        lazyPattern.matcher(annotation).results().forEach(r -> {
            tuple[1] = r.group(1);
        });
        if (tuple[1] == null) {
            tuple[1] = "false";
        }
        convertPattern.matcher(annotation).results().forEach(r -> {
            tuple[2] = r.group(1);
        });
        if (tuple[2] == null) {
            tuple[2] = "";
        }
        recPattern.matcher(annotation).results().forEach(r -> {
            tuple[3] = r.group(1);
        });
        if (tuple[3] == null) {
            tuple[3] = "";
        }
        return tuple;
    }

    public static boolean isItRedirect(String annotation) {
        return Pattern.compile("§redirect\\(module=[$a-zA-Z_]*, *function=[a-zA-Z0-9_-]+, *into=[a-zA-Z0-9_-]*.*|" +
                "§redirect\\(module=[$a-zA-Z_]*, *function=[a-zA-Z0-9_-]+, *into=[a-zA-Z0-9_-]*, *catch.*\\)").matcher(annotation).find();
    }

    public static String[] getModuleFunctionReturnTriple(String  annotation) {
        String[] triple = new String[4];
        Pattern modulePattern = Pattern.compile("§redirect\\(module=([$a-zA-Z_]+).*\\)");
        Pattern functionPattern = Pattern.compile("§redirect\\(.*function=([a-zA-Z0-9_-]+).*\\)");
        Pattern returnPattern = Pattern.compile("§redirect\\(.*into=([a-zA-Z0-9_-]+).*\\)");
        Pattern catchPattern = Pattern.compile("§redirect\\(.*catch(.*)\\)");
        modulePattern.matcher(annotation).results().forEach(r -> {
            triple[0] = r.group(1);
        });
        if (triple[0] == null) {
            triple[0] = "";
        }
        functionPattern.matcher(annotation).results().forEach(r -> {
            triple[1] = r.group(1);
        });
        returnPattern.matcher(annotation).results().forEach(r -> {
            triple[2] = r.group(1);
        });
        if (triple[2] == null) {
            triple[2] = "";
        }
        if (catchPattern.matcher(annotation).find()) {
            triple[3] = "catch";
        } else {
            triple[3] = "no-catch";
        }
        return triple;
    }

    public static boolean isAtom(String annotation) {
        return Pattern.compile("§atom").matcher(annotation).find();
    }

    public static boolean hasUnallowedAtomChars(String atom) {
        return !atom.matches("[a-z][a-zA-Z0-9_\"]+");
    }

    public static String transformAtom(String atom) {
        String replacement = hasUnallowedAtomChars(atom) ? "'" : "";
        return atom.toLowerCase().trim().replace("\"", replacement);
    }

    public static boolean isTlaFunctionCall(List<TLAToken> tokens) {
        StringBuilder wholeString = new StringBuilder();
        tokens.forEach(e -> wholeString.append(e.string));
        return Pattern.compile("(.+)\\((.+,)+\\)|\\((.*)\\)").matcher(wholeString).find();
    }

    public static String getFunctionName(List<TLAToken> tokens) {
        AtomicReference<String> res =  new AtomicReference<>();
        StringBuilder wholeString = new StringBuilder();
        tokens.forEach(e -> wholeString.append(e.string));
        Pattern functionPattern = Pattern.compile("(.+)\\(.*\\)");
        functionPattern.matcher(wholeString.toString()).results().forEach(r -> {
            res.set(r.group(1));
        });
        return res.get();
    }

    public static Pair<String, String> getFunctionArgs(List<TLAToken> tokens, ErlangProcessContext context, TLAExprToErlangStrConverter tlaExprToErlangStrConverter) throws PcalErlangGenException {
        String replacement = "";
        int startParenthesis = tokens.indexOf(tokens.stream().filter(t ->
                t.string.equals("(") && t.type == TLAToken.BUILTIN
        ).toList().get(0));
        int nextParenthesis = tokens.indexOf(tokens.stream().filter(t ->
                t.string.equals(")") && t.type == TLAToken.BUILTIN
        ).toList().get(0));
        int originalSize = tokens.size();
        tokens.remove(startParenthesis);
        tokens.remove(nextParenthesis-1);
        tokens.remove(0);
        if (originalSize-1 != nextParenthesis) {
            // there is an access afterwards
            try {
                nextParenthesis = tokens.indexOf(tokens.stream().filter(t ->
                        t.string.equals("[") && t.type == TLAToken.BUILTIN
                ).toList().get(0));
                replacement = tlaExprToErlangStrConverter.TLAExprToErlangStrInternal(tokens.subList(0, nextParenthesis), context);
            } catch (Exception ignored) {
                PcalDebug.reportInfo("Wired error @ composition ");
            }
        }
        ArrayList<TLAToken> mappedArgs = new ArrayList<>(tokens.stream().map(a -> {
            if (a.type == TLAToken.IDENT) {
                a.string = StringUtils.capitalize(a.string);
            }
            return a;
        }).toList());
        return new Pair<>("$" + replacement + "$", tlaExprToErlangStrConverter.TLAExprToErlangStrInternal(mappedArgs, context));
    }

    public static boolean isAccess(List<TLAToken> tokens) {
        StringBuilder wholeString = new StringBuilder();
        for (TLAToken t : tokens) {
            wholeString.append(t.string);
        }
        Pattern accessListPattern = Pattern.compile("\\w+[\\[\\w+\\]]\\[\\d\\]");
        return accessListPattern.matcher(wholeString.toString()).find();
    }

    private static boolean cEquals(int index, String str, List<TLAToken> tokens) {
        return tokens.get(index).string.equals(str);
    }

    public static String getAccess(List<TLAToken> tokens, String list) {
        List<Integer> indexAccessList = new ArrayList<>();
        for (int i = 0; i < tokens.size(); i++) {
            if (tokens.get(i).type == TLAToken.NUMBER
                    && (cEquals(i-1, "[", tokens) || cEquals(i+1, "]", tokens))) {
                indexAccessList.add(i);
            }
        }
        String res = PCalErlangConstants.NTH_ELEMENT;
        for (int i = indexAccessList.size() - 1; i >= 0; i--) {
            if (i > 0) {
                res = String.format(res, tokens.get(indexAccessList.get(i)).string, PCalErlangConstants.NTH_ELEMENT);
            } else {
                res = String.format(res, tokens.get(indexAccessList.get(i)).string, list);
            }
        }
        return res;
    }
}
