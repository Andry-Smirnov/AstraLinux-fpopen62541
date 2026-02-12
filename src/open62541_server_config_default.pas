{$CODEPAGE UTF8}
// Open62541 Server Config Default
// server_config_default.h
//
// created 2026.02.12

unit open62541_server_config_default;


{$I open62541_definitions.inc}

(*
{$IFDEF FPC}
  {$IFDEF LOAD_DYNAMICALLY}
    {$MODE DELPHI}
    {$HINT DELPHI MODE SELECTED}
  {$ENDIF}
{$ENDIF}
{$H+}
*)


interface


{$IFNDEF LOAD_DYNAMICALLY}
  {$IFDEF ENABLE_SERVER}
// --- server_config_default.h ---

// Create a new server with default plugins for logging etc. used during
// initialization. No network layer and SecurityPolicies are set so far
function UA_Server_new(): PUA_Server; cdecl; external libopen62541;
function UA_ServerConfig_setMinimalCustomBuffer(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString; sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;
  {$ENDIF}
{$ENDIF}


{$IFDEF ENABLE_SERVER}
function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig;
  portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;
{$ENDIF}


implementation


end.

