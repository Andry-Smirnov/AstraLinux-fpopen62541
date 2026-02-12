{$CODEPAGE UTF8}
// Open62541 Server
// server.h
//
// created 2026.02.12

unit open62541_server;


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
  open62541_types
  ;


{$IFDEF ENABLE_SERVER}
type
  UA_Server = record
  end;
  PUA_Server = ^UA_Server;

  UA_MethodCallback = function (
    server: PUA_Server;
    const sessionId: PUA_NodeId;
    sessionContext: Pointer;
    const methodId: PUA_NodeId;
    methodContext: Pointer;
    const objectId: PUA_NodeId;
    objectContext: Pointer;
    inputSize: SIZE_T;
    const input:PUA_Variant;
    outputSize: SIZE_T;
    output:PUA_Variant): UA_StatusCode; cdecl;
{$ENDIF}


implementation

end.

