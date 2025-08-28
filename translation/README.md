<a id="readme-top"></a>
<div align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>
  <h3 align="center">Translation PlusCal to Erlang</h3>
<p><br /></p>
</div>

This directory contains the **translation code** that converts the TLA⁺/PlusCal specification of the GenServer into Erlang code. The translation process is automated and ensures that the generated Erlang code adheres to the semantics defined in the TLA⁺ specification.

## Usage 
1. Set paths in `.env` file
2. Execute `trans.java` class

## Example log
You should see output similar to this:
```shell
java.exe "-javaagent:<IDEA_PATH>\lib\idea_rt.jar=60034" -Dfile.encoding=UTF-8 -Dsun.stdout.encoding=UTF-8 -Dsun.stderr.encoding=UTF-8 -classpath <PROJECT_PATH>\tlatools\src\out\production\tlatools;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.lsp4j-0.15.0.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.lsp4j.generator-0.15.0.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.xtend.lib-2.24.0.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.xtext.xbase.lib-2.24.0.jar;<PROJECT_PATH>\tlatools\src\lib\guava-27.1-jre.jar;<PROJECT_PATH>\tlatools\src\lib\failureaccess-1.0.1.jar;<PROJECT_PATH>\tlatools\src\lib\listenablefuture-9999.0-empty-to-avoid-conflict-with-guava.jar;<PROJECT_PATH>\tlatools\src\lib\jsr305-3.0.2.jar;<PROJECT_PATH>\tlatools\src\lib\checker-qual-2.5.2.jar;<PROJECT_PATH>\tlatools\src\lib\error_prone_annotations-2.2.0.jar;<PROJECT_PATH>\tlatools\src\lib\j2objc-annotations-1.1.jar;<PROJECT_PATH>\tlatools\src\lib\animal-sniffer-annotations-1.17.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.xtend.lib.macro-2.24.0.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.lsp4j.jsonrpc-0.15.0.jar;<PROJECT_PATH>\tlatools\src\lib\gson-2.9.1.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.lsp4j.debug-0.23.1.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.lsp4j.jsonrpc.debug-0.23.1.jar;<PROJECT_PATH>\tlatools\src\lib\org.eclipse.lsp4j.jsonrpc-0.23.1.jar;<PROJECT_PATH>\tlatools\src\lib\gson-2.11.0.jar;<PROJECT_PATH>\tlatools\src\lib\error_prone_annotations-2.27.0.jar;<PROJECT_PATH>\tlatools\src\lib\jline-3.25.1.jar;<PROJECT_PATH>\tlatools\src\lib\javax.mail.glassfish-1.4.1.v201005082020.jar;<PROJECT_PATH>\tlatools\src\lib\javax.activation-1.1.0.v201105071233.jar;<PROJECT_PATH>\tlatools\src\lib\java-dotenv-5.2.1.jar;<PROJECT_PATH>\tlatools\src\lib\kotlin-stdlib-1.3.72.jar;<PROJECT_PATH>\tlatools\src\lib\kotlin-stdlib-common-1.3.72.jar;<PROJECT_PATH>\tlatools\src\lib\annotations-13.0.jar;<PROJECT_PATH>\tlatools\src\lib\commons-io-2.17.0.jar pcal.trans
pcal.trans Version 1.11 of 17 April 2025
      ___           ___       ___           ___           ___           ___           ___                         ___           ___           ___       ___           ___           ___
     /\  \         /\__\     /\__\         /\  \         /\  \         /\  \         /\__\                       /\  \         /\  \         /\__\     /\  \         /\__\         /\  \
    /::\  \       /:/  /    /:/  /        /::\  \       /::\  \       /::\  \       /:/  /                      /::\  \       /::\  \       /:/  /    /::\  \       /::|  |       /::\  \
   /:/\:\  \     /:/  /    /:/  /        /:/\ \  \     /:/\:\  \     /:/\:\  \     /:/  /                      /:/\:\  \     /:/\:\  \     /:/  /    /:/\:\  \     /:|:|  |      /:/\:\  \
  /::\~\:\  \   /:/  /    /:/  /  ___   _\:\~\ \  \   /:/  \:\  \   /::\~\:\  \   /:/  /                      /::\~\:\  \   /::\~\:\  \   /:/  /    /::\~\:\  \   /:/|:|  |__   /:/  \:\  \
 /:/\:\ \:\__\ /:/__/    /:/__/  /\__\ /\ \:\ \ \__\ /:/__/ \:\__\ /:/\:\ \:\__\ /:/__/                      /:/\:\ \:\__\ /:/\:\ \:\__\ /:/__/    /:/\:\ \:\__\ /:/ |:| /\__\ /:/__/_\:\__\
 \/__\:\/:/  / \:\  \    \:\  \ /:/  / \:\ \:\ \/__/ \:\  \  \/__/ \/__\:\/:/  / \:\  \                      \:\~\:\ \/__/ \/_|::\/:/  / \:\  \    \/__\:\/:/  / \/__|:|/:/  / \:\  /\ \/__/
      \::/  /   \:\  \    \:\  /:/  /   \:\ \:\__\    \:\  \            \::/  /   \:\  \                      \:\ \:\__\      |:|::/  /   \:\  \        \::/  /      |:/:/  /   \:\ \:\__\
       \/__/     \:\  \    \:\/:/  /     \:\/:/  /     \:\  \           /:/  /     \:\  \                      \:\ \/__/      |:|\/__/     \:\  \       /:/  /       |::/  /     \:\/:/  /
                  \:\__\    \::/  /       \::/  /       \:\__\         /:/  /       \:\__\                      \:\__\        |:|  |        \:\__\     /:/  /        /:/  /       \::/  /
                   \/__/     \/__/         \/__/         \/__/         \/__/         \/__/                       \/__/         \|__|         \/__/     \/__/         \/__/         \/__/
 ✅ Beginning of algorithm string 5:13 found.
      🏷️ Label 'MONITOR_WAIT_FOR_ACK' in a macro
      🏷️ Label 'S_RECEIVE' in a macro
      🏷️ Label 'C_WAIT_ACK' in a macro
      🏷️ Label 'C_WAIT_ACK_LINK' in a macro
      🏷️ Label 'C_WAIT_ACK_MON' in a macro
      🏷️ Label 'C_WAIT_ACK' in a macro
      🏷️ Label 'STOP_SEND' in a macro
      🏷️ Label 'STOP_RECEIVE' in a macro
      🏷️ Label 'STOP_DEMON' in a macro
      🏷️ Label 'CALL_SEND' in a macro
      🏷️ Label 'CALL_RECEIVE' in a macro
      🏷️ Label 'CALL_DEMON' in a macro
Transforming label: MONITOR_WAIT_FOR_ACK with appendix: _monitor
Transforming label: C_WAIT_ACK_MON with appendix: _genStart6
Transforming label: C_WAIT_ACK_LINK with appendix: _genStart6
Transforming label: C_WAIT_ACK with appendix: _genStart6
Transforming label: C_WAIT_ACK with appendix: _genStart5
Transforming label: C_WAIT_ACK_MON_genStart6 with appendix: _genStart6
Transforming label: C_WAIT_ACK_LINK_genStart6 with appendix: _genStart6
Transforming label: C_WAIT_ACK_genStart6 with appendix: _genStart6
Transforming label: C_WAIT_ACK_genStart5 with appendix: _genStart5
Transforming label: MONITOR_WAIT_FOR_ACK_monitor with appendix: _genCall
Transforming label: CALL_RECEIVE with appendix: _genCall
Transforming label: CALL_RECEIVE with appendix: _genCall
Transforming label: CALL_SEND with appendix: _genCall
Transforming label: CALL_DEMON with appendix: _genCall
      🏷️ Label 'ABCAST_WHILE' in a macro
Transforming label: S_RECEIVE with appendix: _receiveSignal
Transforming label: S_RECEIVE_receiveSignal with appendix: _receiveSignal
Transforming label: MONITOR_WAIT_FOR_ACK_monitor_genCall with appendix: _call3
Transforming label: CALL_RECEIVE_genCall with appendix: _call3
Transforming label: CALL_SEND_genCall with appendix: _call3
Transforming label: CALL_DEMON_genCall with appendix: _call3
Transforming label: MONITOR_WAIT_FOR_ACK with appendix: _stop3
Transforming label: STOP_RECEIVE with appendix: _stop3
Transforming label: STOP_RECEIVE with appendix: _stop3
Transforming label: STOP_SEND with appendix: _stop3
Transforming label: STOP_DEMON with appendix: _stop3
      🏷️ Label 'MONITOR_WAIT_FOR_ACK' in a macro
      🏷️ Label 'S_RECEIVE' in a macro
      🏷️ Label 'C_WAIT_ACK' in a macro
      🏷️ Label 'C_WAIT_ACK_LINK' in a macro
      🏷️ Label 'C_WAIT_ACK_MON' in a macro
      🏷️ Label 'C_WAIT_ACK' in a macro
      🏷️ Label 'STOP_SEND' in a macro
      🏷️ Label 'STOP_RECEIVE' in a macro
      🏷️ Label 'STOP_DEMON' in a macro
Transforming label: MONITOR_WAIT_FOR_ACK with appendix: _monitor
      🏷️ Label 'CALL_SEND' in a macro
      🏷️ Label 'CALL_RECEIVE' in a macro
      🏷️ Label 'CALL_DEMON' in a macro
Transforming label: MONITOR_WAIT_FOR_ACK_monitor with appendix: _monitor
      🏷️ Label 'ABCAST_WHILE' in a macro
 ✅ Parsing completed.
Warning: symbols were renamed.
 ✅ PlusCal translation completed.
 ✅ Started Erlang translation.
      🔴 removing [label |-> "S_WAIT_SPAWN", st...
      🔴 removing [label |-> "S_RELEASE_RESOURCE...
      🔴 skipping erlang generation for labels [S_WAIT_SPAWN, S_EMIT_MONITOR_DOWN, S_RELEASE_RESOURCES]
      ℹ️ Result not saved proc_lib:hibernate
      ℹ️ Result not saved sys:handle_system_msg
      ℹ️ Result not saved sys:handle_system_msg
      ℹ️ Result not saved util:terminate
      ℹ️ Result not saved util:terminate
      ℹ️ Result not saved util:terminate
      ℹ️ Result not saved util:terminate
      ℹ️ Result not saved util:terminate
      ℹ️ Result not saved util:terminate
      ℹ️ Result not saved util:terminate
      ℹ️ dangling tuple_to_list/2
      🔴 skipping erlang generation for process workerClient
      🔴 skipping erlang generation for process controlClient
      🔴 skipping erlang generation for process timer
      🔴 skipping definition "unalias". Expected second token to be "==", but was (.
      🔴 skipping definition "receiveSelective". Expected second token to be "==", but was (.
      🔴 skipping definition "receivedDown". Expected second token to be "==", but was (.
      🔴 skipping definition "list_to_tuple". Expected second token to be "==", but was (.
      🔴 skipping definition "tuple_to_list". Expected second token to be "==", but was (.
      🔴 [Parent, Name, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, Continue] not used in body, adding as _Debug
      ℹ️ Result not saved :loop_0
      🔴 [Reason] not used in body, adding as _Parent
      ℹ️ Result not saved util:terminate
      🔴 [Parent] not used in body, adding as _Debug
      ℹ️ Result not saved util:continue
      🔴 skipping reqStart = true
      🔴 skipping reqStart = true
      🔴 skipping reqStart = true
      🔴 skipping reqStart = true
      🔴 skipping reqStart = true
      🔴 skipping reqStart = true
      ℹ️ Result not saved gen:stop
      ℹ️ Result not saved erlang:exit
      ℹ️ Result not saved erlang:exit
      ℹ️ Result not saved gen:reply
      ℹ️ Result not saved global:send
      ℹ️ Result not saved $module:send
      ℹ️ Result not saved erlang:send
      ℹ️ Result not saved erlang:send
      🔴 skipping abcastNodes = RValTemp
      ℹ️ Result not saved erlang:send
 ✅ Erlang translation completed.
      📝 New file <PROJECT_PATH>\tlatools\src\.resources\gen_server_behaviour_simple.tla written.
      📝 New file <PROJECT_PATH>\tlatools\src\.resources\erla_libs_comm.erl written.
      📝 New file <PROJECT_PATH>\tlatools\src\.resources\erla_libs_sets.erl written.
      📝 New file <PROJECT_PATH>\tlatools\src\.resources\erla_libs_util.erl written.
      📝 New file <TARGET_PATH>\broadcast\src\common\gen_server_generation\gen_server_behaviour_simple.erl written.
      📝 New file <TARGET_PATH>\broadcast\src\common\gen_server_generation\gen_server_behaviour_simple.hrl written.
 ✅ Typer on gen_server_behaviour_simple.erl exited with code 0
      ℹ️ Augmenting 21 functions with inferred types
      ℹ️ Augmented 21/21 functions
      📝 Overwritten file <TARGET_PATH>\broadcast\src\common\gen_server_generation\gen_server_behaviour_simple.erl
      📝 New file <TARGET_PATH>\broadcast\src\common\gen_server_generation\gen_server_behaviour_simple.erl written.
      📝 File <PROJECT_PATH>\tlatools\src\.resources\gen_server_behaviour_simple.cfg already contains SPECIFICATION statement, so new one not written.
      📝 New file <PROJECT_PATH>\tlatools\src\.resources\gen_server_behaviour_simple.cfg written.

Process finished with exit code 0
```
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>