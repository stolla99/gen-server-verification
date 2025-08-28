package pcal.postprocessor;

import pcal.PcalDebug;
import pcal.PcalErlangGen;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import static pcal.trans.ERLANG_TAKE_PARENT;
import static pcal.trans.ERLANG_TARGET;

public class Typer {
    private final String SPEC_DETECTION = "\\s*-spec.*";
    private final String FUNCTION_DETECTION = "\\s*-spec\\s(\\w*)\\(.*";
    private final String FUNCTION_DEF_DETECTION = "\\(.*\\)\\s*->";

    public List<String> executePostprocessing(Vector<String> erlangCode) {
        try {
            ProcessBuilder builder = new ProcessBuilder();

            boolean windowsOs = System.getProperty("os.name").contains("Windows");
            String library = "typer";
            String path = (ERLANG_TAKE_PARENT ? ERLANG_TARGET : "") + "\\";
            String erlangFileName = PcalErlangGen.getErlangFileNameOnly();
            String typerCommand = library + " " + erlangFileName;
            if (windowsOs) {
                builder.command("cmd.exe", "/c", "cd " + path + " && " + typerCommand);
            } else {
                builder.command("sh", "-c", "cd " + path + " && " + typerCommand);
            }
            Process process = builder.start();
            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            ArrayList<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            int exitCode = process.waitFor();
            PcalDebug.reportSuccess("Typer on " + erlangFileName + " exited with code " + exitCode);
            List<String> linesFiltered = lines.stream().filter(e -> Pattern
                    .compile(SPEC_DETECTION)
                    .matcher(e).find()).toList();
            PcalDebug.reportInfo("      ℹ️ Augmenting " + linesFiltered.size() + " functions with inferred types");
            int from = 0;
            int successCount = 0;
            for (String spec : linesFiltered) {
                int lineToUpdate = findLineOfSpec(spec, erlangCode, from);
                if (lineToUpdate == -1) {
                    from = 0;
                } else {
                    // skip the current line as it was already augmented
                    erlangCode.insertElementAt(spec, lineToUpdate);
                    from = lineToUpdate + 2;
                    successCount++;
                }
            }
            PcalDebug.reportInfo("      ℹ️ Augmented " + successCount + "/" + linesFiltered.size() + " functions");
        } catch (Exception e) {
            PcalDebug.reportError("Typer on " + ERLANG_TARGET + " exited with error: " + e.getMessage());
        }
        return erlangCode;
    }

    private int findLineOfSpec(String spec, List<String> erlangCode, int from) {
        for (int i = from; i < erlangCode.size(); i++) {
            if (isFuncDefOf(getFuncName(spec), erlangCode.get(i))) {
                return i;
            }
        }
        return -1;
    }

    private boolean isFuncDefOf(String func, String codeLine) {
        return Pattern
                .compile(".*" + func + FUNCTION_DEF_DETECTION)
                .matcher(codeLine)
                .find();
    }

    private String getFuncName(String spec) {
        AtomicReference<String> result = new AtomicReference<>("");
        Pattern namePattern = Pattern.compile(FUNCTION_DETECTION);
        namePattern.matcher(spec).results().forEach(r -> {
            result.set(r.group(1));
        });
        return result.get();
    }
}
