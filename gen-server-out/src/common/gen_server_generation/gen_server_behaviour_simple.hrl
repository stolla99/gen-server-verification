
-define(const_WorkerClients, change_me).
-define(const_ServerSet, ?const_Server).
-define(const_Server, change_me).
-define(const_null, change_me).
-define(const_NodeSet, util:ServerSet++(++?const_WorkerClients++?const_ControlClients)).
-define(const_default_timeout, 5000).
-define(const_ControlClients, change_me).
-define(const_AllClients, ?const_WorkerClients++?const_ControlClients).

-record(state_genServer, {
    procvar_ID = -1,
    var_smsg = ?const_null,
    var_raw_msg = ?const_null,
    var_serverUp = false,
    var_serverState = [],
    var_timeout = ?const_null,
    var_hibernate = "",
    var_continue = [],
    var_hibernateAfterTimeout = ?const_null,
    var_skipLoop = false,
    var_skipLoopHibernate = false,
    var_skipWait = false,
    var_parent = "",
    var_name = "",
    var_initResult = ?const_null,
    var_module = 'gen_server_callbacks'
}).