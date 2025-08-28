package pcal;

public class Global {
    private static int moduleCounter = 0;

    private static int resCounter = 0;

    public static int increaseAndGetModuleCounter() {
        moduleCounter++;
        return moduleCounter-1;
    }

    public static int increaseAndGetResCounter() {
        resCounter++;
        return resCounter-1;
    }
}
