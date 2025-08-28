package pcal;

import java.util.List;

public final class PCalErlangConstants {
    private PCalErlangConstants() {
        // hide constructor, since it should not be instantiated
    }

    // todo: extract strings from PCalErlangGen to here (low priority)
    // todo: make placeholders in template strings more readable by using e.g.
    //  StringSubstitute From Apache Commons Text.

    public static String GLOBAL_QUEUES_NAME = "queues";
    public static String PREFIX_RETURN_VALUE = "rVal";

    public static int NUM_SPACES_FOR_INDENT = 4;

    public static String MODULE_DECL = "-module(%s)";
    public static String INCLUDE_DECL = "-include(%s)";
    public static String EXPORT_DECL = "-export[]";
    public static String EXPORT_PLAIN = "-export";
    public static String RECORD_DECL = "-record(%s, "; //{%s}) is done by parser

    public static String PREFIX_PROCESS_STATE_NAME = "state_";
    public static String PREFIX_VAR = "var_";
    public static String PREFIX_CONSTANT_VAR = "const_";
    public static String PREFIX_INTERNAL_PROC_VAR = "procvar_";
    public static String PREFIX_RECORD_KEY = ""; // is not needed
    public static String PREFIX_FUNCTION_WHILE = "loop_";
    public static String PREFIX_PROCESS = "process_";

    public static String PROCESS_STATE_VAR_NAMES = "State%o";
    public static String MESSAGE_VAR_NAMES = "Message%o";
    public static String TEMP_VAR_NAMES = "T%s%o";


    /** A template string for accessing a record's field.
     *  Template: Var#record.field.
     */
    public static String PROCESS_FIELD_ACCESS = "%s#%s.%s";
    public static String PROCESS_FIELD_ACCESS_INPLACE = "#%s.%s";
    /**
     * A template string for variable assignment.
     *  Template: NewVar = OldVar#record{field = value}
     */
    public static String PROCESS_VAR_ASSIGNMENT = "%s = %s#%s{%s = %s}";
    public static String PROCESS_VAR_ASSIGNMENT_INPLACE = "#%s{%s = %s}";
    public static String TEMP_VAR_ASSIGNMENT= "%s = %s";
    public static char COMMENT_CHAR = '%';

    public static String UTIL_MODULE_NAME = "util";

    public static String SINGLE_LINE_COMMENT = COMMENT_CHAR + Character.toString(COMMENT_CHAR) + " %s"; // escape % char for formatting

    public static String COMMENT_BAR_SEPARATOR = "%%%=========================================================================";
    public static String COMMENT_BAR_INNER_TEXT = "%%% ";

    public static String ENTRY_POINT_NAME = "loop";
    public static String ENTRY_POINT_ARGS = "Name, Parent, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, Continue";
    public static String LOOP_ENTRY = "loop_0";
    public static String ENTRY_POINT_BODY_START = "#state_genServer{";
    public static String ENTRY_POINT_BODY = "var_serverState = State, " +
            "var_serverUp = true, " +
            "var_name = Name, " +
            "var_parent = Parent, " +
            "var_module = Mod, " +
            "var_hibernateAfterTimeout = HibernateAfterTimeout, " +
            "var_timeout = Timeout, " +
            "var_hibernate = Hibernate, " +
            "var_continue = Continue";
    public static String ENTRY_POINT_BODY_END = "}";

    /**
     * Lists
     */
    public static String NTH_ELEMENT = "lists:nth(%s, %s)";
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /*
        Constants
     */
    public static final String IS_TEST = "IS_TEST";
    public static final String MACRO_INIT_VALUE = "change_me";
    public static String MACRO_DEFINITION = "-define(%s, %s).";
    public static String MACRO_USAGE = "?%s";

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /*
        Erlang functions
     */

    public static String PRINT_STATEMENT =  "io:format(\"~p~n\", [%s])";
    public static String INITIAL_FUNCTION_DECL = "maps:from_list(lists:map(%s, %s))";
    public static String SET_FILTER = "sets:filter(fun(%s) -> %s end, %s)";

    public static String ERLA_LIBS_MODULE_COMMUNICATION_NAME = "erla_libs_comm";
    public static String ERLA_LIBS_MODULE_SETS_NAME = "erla_libs_sets";
    public static String ERLA_LIBS_MODULE_UTIL_NAME = "erla_libs_util";

    public static List<String> SEND_ALIAS = List.of("send", "sendTo");
    public static List<String> BROADCAST_ALIAS = List.of("broadcast");
    public static String SEND_STATEMENT = ERLA_LIBS_MODULE_COMMUNICATION_NAME + ":send(%s, %s)"; // erla_libs:send(receiver, message)
    public static String BROADCAST_STATEMENT = ERLA_LIBS_MODULE_COMMUNICATION_NAME + ":broadcast(%s)"; // erla_libs:broadcast(message)
    public static String REGISTER_STATEMENT = ERLA_LIBS_MODULE_COMMUNICATION_NAME + ":register_proc(%s)";
    public static String RECORD_ACCESS_STATEMENT = ERLA_LIBS_MODULE_UTIL_NAME + ":get_nested_value(%s, [%s])";
    public static String RECORD_ACCESS_ASSIGNMENT = ERLA_LIBS_MODULE_UTIL_NAME + ":update_nested_value(%s, [%s], %s)";

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /*
        Main function
     */
    public static String PREFIX_PROCESS_START_FUNC = "start_";
    public static String START_FUNC_INIT_PROCESS_SET = "[%s(Id) || Id <- sets:to_list(%s)]";

    /**
     * Send operation types.
     */
    public enum SendType {
        SEND,
        BROADCAST;

        @Override
        public String toString() {
            return switch (this) {
                case SEND -> "send";
                case BROADCAST -> "broadcast";
            };
        }
    }
}
