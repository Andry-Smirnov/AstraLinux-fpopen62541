{$CODEPAGE UTF8}
// Open62541 Client HighLevel
// client_highlevel.h
//
// created 2026.02.12

unit open62541_client_highlevel;


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
function __UA_Client_readAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_readArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: size_t; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;

function __UA_Client_writeAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_writeArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: size_t; newArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;
  {$IFDEF UA_ENABLE_METHODCALLS}
function UA_Client_call(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: size_t; input: PUA_Variant; out outputSize: size_t; out output: PUA_Variant): UA_StatusCode; cdecl; external libopen62541;
  {$ENDIF}
{$ENDIF}

implementation


end.

