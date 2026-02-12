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


  {$IFNDEF LOAD_DYNAMICALLY}
procedure UA_Server_delete(server: PUA_Server); cdecl; external libopen62541;
function UA_Server_getConfig(server: PUA_Server): PUA_ServerConfig; cdecl; external libopen62541;

// Runs the main loop of the server. In each iteration, this calls into the
// networklayers to see if messages have arrived.
// @param server The server object.
// @param running The loop is run as long as *running is True.
//        Otherwise, the server shuts down.
// @return Returns the statuscode of the UA_Server_run_shutdown method
function UA_Server_run(server: PUA_Server; running: PUA_Boolean): UA_StatusCode; cdecl; external libopen62541;

// The prologue part of UA_Server_run (no need to use if you call UA_Server_run)
function UA_Server_run_startup(server: PUA_Server): UA_StatusCode; cdecl; external libopen62541;
// Executes a Single iteration of the server's main loop. *)
function UA_Server_run_iterate(server: PUA_Server;
  waitInternal: UA_Boolean): UA_UInt16; cdecl; external libopen62541;
// The epilogue part of UA_Server_run (no need to use if you call UA_Server_run)
function UA_Server_run_shutdown(server: PUA_Server): UA_StatusCode; cdecl; external libopen62541;

function __UA_Server_addNode(server: PUA_Server; const nodeClass: UA_NodeClass;
  const requestedNewNodeId: PUA_NodeId;
  const parentNodeId: PUA_NodeId;
  const referenceTypeId: PUA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: PUA_NodeId;
  const attr: PUA_NodeAttributes;
  const attributeType: PUA_DataType;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;  external libopen62541;
function UA_Server_addReference(server: PUA_Server; const sourceId: UA_NodeId;
  const refTypeId: UA_NodeId;
  const targetId: UA_ExpandedNodeId;
  isForward: UA_Boolean): UA_StatusCode; cdecl; external libopen62541;
function UA_Server_deleteReference(server: PUA_Server; const sourceNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId; isForward: UA_Boolean;
  const targetNodeId: UA_ExpandedNodeId;
  deleteBitirectional: UA_Boolean): UA_StatusCode; cdecl; external libopen62541;
function __UA_Server_write(server: PUA_Server; const nodeId: PUA_NodeId;
  const attributeId: UA_AttributeId; const attr_type: PUA_DataType;
  attr: Pointer): UA_StatusCode; cdecl; external libopen62541;
function UA_Server_addNamespace: function (server: PUA_Server;
  namespace: PChar):UA_Uint16; cdecl; external libopen62541;
function UA_Server_addMethodNodeEx: function(server: PUA_Server;
  const requestedNewNodeId: PUA_NodeId;
  const parentNodeId: PUA_NodeId;
  const referenceTypeId: PUA_NodeId;
  const browseName:PUA_QualifiedName ;
  const attr:PUA_MethodAttributes; method: UA_MethodCallback;
  inputArgumentsSize: size_t; const inputArguments: PUA_Argument;
  const inputArgumentsRequestedNewNodeId: PUA_NodeId;
  inputArgumentsOutNewNodeId: PUA_NodeId;
  outputArgumentsSize: size_t; const outputArguments: PUA_Argument;
  const outputArgumentsRequestedNewNodeId: PUA_NodeId;
  outputArgumentsOutNewNodeId: PUA_NodeId;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;  external libopen62541;
  {$ENDIF}

{$ENDIF}

{$IFDEF ENABLE_SERVER}
function UA_Server_addVariableNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: UA_NodeId;
  const attr: UA_VariableAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addObjectTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr:UA_ObjectTypeAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addObjectNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: UA_NodeId;
  const attr:UA_ObjectAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addVariableTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: UA_NodeId;
  const attr:UA_VariableTypeAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addViewNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr:UA_ViewAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addReferenceTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr: UA_ReferenceTypeAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addDataTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr:UA_DataTypeAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_writeValue(server: PUA_Server; const nodeId: UA_NodeId;
  const value: UA_Variant): UA_StatusCode;
function UA_Server_addMethodNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName ;
  const attr: UA_MethodAttributes;
  method: UA_MethodCallback;
  inputArgumentsSize: size_t;
  const inputArguments: PUA_Argument;
  outputArgumentsSize: size_t;
  const outputArguments: PUA_Argument;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
{$ENDIF}


implementation

end.

