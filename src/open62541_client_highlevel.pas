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


uses
  open62541,
  open62541_types_h,
//  open62541_types_generated,
  open62541_client,
  open62541_common
  ;


{$IFDEF LOAD_DYNAMICALLY}
var
  __UA_Client_readAttribute: function(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl;

{$ELSE}

function __UA_Client_readAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_readArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: size_t; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;

function __UA_Client_writeAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_writeArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: size_t; newArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;
  {$IFDEF UA_ENABLE_METHODCALLS}
function UA_Client_call(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: size_t; input: PUA_Variant; out outputSize: size_t; out output: PUA_Variant): UA_StatusCode; cdecl; external libopen62541;
  {$ENDIF}
{$ENDIF}


function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValue: UA_Variant): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValue: Byte): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValue: SmallInt): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValue: Longint): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValue: AnsiString): UA_StatusCode; overload;
function UA_Client_readDataTypeAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outDataType: UA_NodeId): UA_StatusCode;
function UA_Client_readValueRankAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValueRank: UA_Int32): UA_StatusCode;
function UA_Client_readBrowseNameAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outBrowseName: UA_QualifiedName): UA_StatusCode;
function UA_Client_readDisplayNameAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outDisplayName: UA_LocalizedText): UA_StatusCode;
function UA_Client_readDescriptionAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outDescription: UA_LocalizedText): UA_StatusCode;

function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newValue: UA_Variant): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValue: Byte): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValue: SmallInt): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValue: Longint): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValue: AnsiString): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValues: array of AnsiString): UA_StatusCode; overload;
function UA_Client_writeDescriptionAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newDescription: UA_LocalizedText): UA_StatusCode;
function UA_Client_writeDataTypeAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; newDataType: PUA_NodeId): UA_StatusCode;
function UA_Client_writeValueRankAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValueRank: UA_Int32): UA_StatusCode;

{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
function UA_MonitoredItemCreateRequest_default(nodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
{$ENDIF}


implementation


uses
  open62541_types,
//  open62541_statuscodes,
  open62541_types_generated_handling
  ;


function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outValue: UA_Variant): UA_StatusCode; overload;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @outValue, @UA_TYPES[UA_TYPES_VARIANT]);
end;


{$IFDEF FPC} {$WARN 5058 OFF : Variable "$1" does not seem to be initialized} {$ENDIF}
// taken from ua_client_highlevel.c: __UA_Client_readAttribute()
// (use to read subrange of array variable)
function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const indexRange: AnsiString;
  out outValue: UA_Variant): UA_StatusCode; overload;
var
  Item: UA_ReadValueId;
  Request: UA_ReadRequest;
  Response: UA_ReadResponse;
begin
  FillChar(Item, sizeof(UA_ReadValueId), #0);
  Item.nodeId := nodeId;
  Item.attributeId := ord(UA_ATTRIBUTEID_VALUE);
  Item.indexRange := _UA_STRING_ALLOC(indexRange);
  FillChar(Request, sizeof(UA_ReadRequest), #0);
  Request.nodesToRead := @Item;
  Request.nodesToReadSize := 1;
  Response := UA_Client_Service_read(client, Request);
  Result := Response.responseHeader.serviceResult;
  if Result = UA_STATUSCODE_GOOD then
    begin
      if Response.resultsSize = 1 then
        Result := Response.results[0].status
      else
        Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
    end;

  if Result = UA_STATUSCODE_GOOD then
    begin
      (* Set the StatusCode *)
      if Response.results^.flag and 2 <> 0 then
        Result :=  Response.results^.status;

      (* Return early of no value is given *)
      if Response.results^.flag and 1 <> 0 then
        begin
          (* Copy value into out *)
           outValue := Response.results^.value;
           UA_Variant_init(Response.results^.value);
        end
      else
        Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
    end;

  UA_clear(@Response, @UA_TYPES[UA_TYPES_READRESPONSE]);
  UA_String_clear(Item.indexRange);
end;
{$IFDEF FPC} {$WARN 5058 ON : Variable "$1" does not seem to be initialized} {$ENDIF}


function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outValue: Byte): UA_StatusCode; overload;
var
  Value: UA_Variant;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @Value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then
    begin
      if UA_Variant_hasScalarType(@Value, @UA_TYPES[UA_TYPES_BYTE]) then
        outValue:= PUA_Byte(Value.data)^
      else
        Result := UA_STATUSCODE_BADTYPEMISMATCH;
    end;
  UA_Variant_clear(Value);
end;


function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outValue: SmallInt): UA_StatusCode; overload;
var
  Value: UA_Variant;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @Value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then
    begin
      if UA_Variant_hasScalarType(@Value, @UA_TYPES[UA_TYPES_INT16]) then
        outValue:= PUA_Int16(Value.data)^
      else
        Result := UA_STATUSCODE_BADTYPEMISMATCH;
    end;
  UA_Variant_clear(Value);
end;


function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outValue: Longint): UA_StatusCode; overload;
var
  Value: UA_Variant;
begin
  UA_Variant_init(Value);
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @Value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then
    begin
      if UA_Variant_hasScalarType(@Value, @UA_TYPES[UA_TYPES_INT32]) then
        outValue:= PUA_Int32(Value.data)^
      else
        Result := UA_STATUSCODE_BADTYPEMISMATCH;
    end;
  UA_Variant_clear(Value);
end;


function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outValue: AnsiString): UA_StatusCode; overload;
var
  Value: UA_Variant;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @Value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then
    begin
      if UA_Variant_hasScalarType(@Value, @UA_TYPES[UA_TYPES_STRING]) then
        SetString(outValue, PAnsiChar(PUA_String(Value.data)^.data), PUA_String(Value.data)^.length)
      else
        Result := UA_STATUSCODE_BADTYPEMISMATCH;
    end;
  UA_Variant_clear(Value);
end;


function UA_Client_readDataTypeAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outDataType: UA_NodeId): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DATATYPE,
    @outDataType, @UA_TYPES[UA_TYPES_NODEID]);
end;


function UA_Client_readValueRankAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outValueRank: UA_Int32): UA_StatusCode;
begin
  // -2:Any; -1:Scalar; 0:OneOrMoreDimensions; 1:OneDimension
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUERANK,
    @outValueRank, @UA_TYPES[UA_TYPES_INT32]);
end;


function UA_Client_readBrowseNameAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outBrowseName: UA_QualifiedName): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_BROWSENAME,
    @outBrowseName, @UA_TYPES[UA_TYPES_QUALIFIEDNAME]);
end;


function UA_Client_readDisplayNameAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outDisplayName: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DISPLAYNAME,
    @outDisplayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;


function UA_Client_readDescriptionAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  out outDescription: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DESCRIPTION,
    @outDescription, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;


function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newValue: UA_Variant): UA_StatusCode; overload;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @newValue, @UA_TYPES[UA_TYPES_VARIANT]);
end;


function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: Byte): UA_StatusCode; overload;
var
  UAVar: UA_Variant;
begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_BYTE]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;


function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: SmallInt): UA_StatusCode; overload;
var
  UAVar: UA_Variant;
begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_INT16]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;


function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: Longint): UA_StatusCode; overload;
var
  UAVar: UA_Variant;
begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_INT32]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;


function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: AnsiString): UA_StatusCode; overload;
var
  UAVar: UA_Variant;
  UAStr: UA_String;
begin
  UAStr := _UA_STRING_ALLOC(newValue);
  UA_Variant_setScalar(@UAVar, @UAStr, @UA_TYPES[UA_TYPES_STRING]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
  UA_String_clear(UAStr);
end;


{$IFDEF FPC} {$WARN 5058 OFF : Variable "$1" does not seem to be initialized} {$ENDIF}
function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValues: array of AnsiString): UA_StatusCode; overload;
var
  UAVar: UA_Variant;
  UAStr: array of UA_String;
  i: Integer;
begin
  SetLength(UAStr, Length(newValues));
  for i := Low(newValues) to High(newValues) do
    UAStr[i] := _UA_STRING_ALLOC(newValues[i]);
  UA_Variant_setArray(@UAVar, @UAStr[0], Length(UAStr), @UA_TYPES[UA_TYPES_STRING]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
  for i := Low(newValues) to High(newValues) do
    UA_String_clear(UAStr[i]);
end;
{$IFDEF FPC} {$WARN 5058 ON : Variable "$1" does not seem to be initialized} {$ENDIF}


function UA_Client_writeDescriptionAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newDescription: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_DESCRIPTION,
    @newDescription, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;


function UA_Client_writeDataTypeAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  newDataType: PUA_NodeId): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_DATATYPE,
    newDataType, @UA_TYPES[UA_TYPES_NODEID]);
end;


function UA_Client_writeValueRankAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValueRank: UA_Int32): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUERANK,
    @newValueRank, @UA_TYPES[UA_TYPES_INT32]);
end;


{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
begin
  UA_CreateSubscriptionRequest_init(Result);

  // in ms
  Result.requestedPublishingInterval := 500.0;
  // the number of PublishingIntervals to wait for a new PublishRequest,
  // before realizing that the client is no longer active
  Result.requestedLifetimeCount := 10000;
  // how many intervals may be skipped, before an empty notification is sent
  Result.requestedMaxKeepAliveCount := 10;
  // unlimited
  Result.maxNotificationsPerPublish := 0;
  Result.publishingEnabled := True;
  Result.priority := 0;
end;


function UA_MonitoredItemCreateRequest_default(nodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
begin
  UA_MonitoredItemCreateRequest_init(Result);

  Result.itemToMonitor.nodeId := nodeId;
  Result.itemToMonitor.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  Result.monitoringMode := UA_MONITORINGMODE_REPORTING;
  Result.requestedParameters.samplingInterval := 250;
  Result.requestedParameters.discardOldest := True;
  Result.requestedParameters.queueSize := 1;
end;
{$ENDIF}


end.

