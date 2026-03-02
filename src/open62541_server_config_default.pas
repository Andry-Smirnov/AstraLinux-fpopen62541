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


uses
  open62541_types_h,
  open62541_server_config
  ;


{$IFNDEF LOAD_DYNAMICALLY}
// --- server_config_default.h ---

// Create a new server with default plugins for logging etc. used during
// initialization. No network layer and SecurityPolicies are set so far
function UA_Server_new(): PUA_Server; cdecl; external libopen62541;
function UA_ServerConfig_setMinimalCustomBuffer(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString; sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}


function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig;
  portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;


implementation


uses
  open62541_types
  ;


// Creates a server config on the default port 4840 with no server certificate.
function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimal(config, 4840, nil);
end;


// Creates a new server config with one endpoint.
// The config will set the tcp network layer to the given port and adds a Single
// endpoint with the security policy ``SecurityPolicy#None`` to the server. A
// server certificate may be supplied but is optional.
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig;
  portNumber: UA_UInt16;
  const certificate: PUA_ByteString): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimalCustomBuffer(config, portNumber, certificate, 0, 0);
end;


end.

