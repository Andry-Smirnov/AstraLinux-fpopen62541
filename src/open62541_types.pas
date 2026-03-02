{$CODEPAGE UTF8}
// Open62541 Types
// types.c
//
// created 2026.02.12

unit open62541_types;


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
  SysUtils,
  // ---
  open62541_const,
  open62541_types_h,
  open62541_types_generated,
  open62541_client,
  open62541_client_config,
  open62541_client_subscriptions,
  open62541_common,
  open62541_server,
  open62541_server_config,
  open62541_statuscodes,
  open62541_pki
  ;


{$IFDEF LOAD_DYNAMICALLY}
var
  UA_VariableAttributes_default: UA_VariableAttributes;
  UA_ObjectAttributes_default: UA_ObjectAttributes;
  UA_ObjectTypeAttributes_default: UA_ObjectTypeAttributes;
  UA_ReferenceTypeAttributes_default: UA_ReferenceTypeAttributes;
  UA_DataTypeAttributes_default: UA_DataTypeAttributes;

  UA_StatusCode_name: function(code: UA_StatusCode): PAnsiChar; cdecl;
  UA_String_fromChars: function(src: PAnsiChar): UA_String; cdecl;
  UA_String_equal: function(const s1, s2: PUA_String): UA_Boolean; cdecl;
  UA_NodeId_isNull: function (p: PUA_NodeId): UA_Boolean; cdecl;
  UA_NodeId_print: function(id: PUA_NodeId; output: PUA_String): UA_StatusCode; cdecl;
  UA_NumericRange_parse: function(range: PUA_NumericRange; const str: UA_String): UA_StatusCode; cdecl;
  UA_Variant_setScalar: procedure(v: PUA_Variant; p: Pointer; _type: PUA_DataType); cdecl;
  UA_Variant_setScalarCopy: function(v: PUA_Variant; p: Pointer; _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_Variant_setArray: procedure(v: PUA_Variant; arrayData: Pointer; arraySize: size_t; _type: PUA_DataType); cdecl;
  UA_Variant_setArrayCopy: function(v: PUA_Variant; arrayData: Pointer; arraySize: size_t; _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_DateTime_toStruct: function(t: UA_DateTime): UA_DateTimeStruct; cdecl;
  UA_DateTime_fromStruct: function(ts: UA_DateTimeStruct): UA_DateTime; cdecl;
  UA_findDataType: function(typeId: PUA_NodeId): PUA_DataType; cdecl;

  UA_new: function(const _type: PUA_DataType): Pointer; cdecl;
  UA_copy: function(src,dst: Pointer; const _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_clear: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_delete: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_Array_delete: procedure(p: Pointer; size: size_t; const _type: PUA_DataType); cdecl;

  UA_Client_new: function (): PUA_Client; cdecl;
  UA_Client_newWithConfig: function(const config: PUA_ClientConfig): PUA_Client; cdecl;
  UA_Client_getState: procedure(client: PUA_Client; channelState: PUA_SecureChannelState; sessionState: PUA_SessionState; connectStatus: PUA_StatusCode); cdecl;
  UA_Client_getConfig: function(client: PUA_Client): PUA_ClientConfig; cdecl;
  UA_ClientConfig_setDefault: function(config: PUA_ClientConfig): UA_StatusCode; cdecl;
  UA_ClientConfig_setDefaultEncryption: function(config: PUA_ClientConfig; localCertificate, privateKey: UA_ByteString; trustList: PUA_ByteString; trustListSize: size_t; revocationList: PUA_ByteString; revocationListSize: size_t): UA_StatusCode; cdecl;
  UA_CertificateVerification_AcceptAll: procedure(cv : PUA_CertificateVerification); cdecl;
  UA_Client_delete: procedure(client: PUA_Client); cdecl;
  UA_Client_connect: function(client: PUA_Client; const endpointUrl: AnsiString): UA_StatusCode; cdecl;
  UA_Client_disconnect: function(client: PUA_Client): UA_StatusCode; cdecl;
  __UA_Client_Service: procedure(client: PUA_Client; const request: Pointer; const requestType: PUA_DataType; response: Pointer; const responseType: PUA_DataType); cdecl;
  UA_Client_run_iterate: function(client: PUA_Client; timeout: UA_UInt32): UA_StatusCode; cdecl;

  UA_Client_readArrayDimensionsAttribute: function(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: size_t; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl;

  __UA_Client_writeAttribute: function(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl;
  UA_Client_writeArrayDimensionsAttribute: function(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: size_t; newArrayDimensions: PUA_UInt32): UA_StatusCode;
  {$IFDEF UA_ENABLE_METHODCALLS}
  UA_Client_call: function(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: size_t; input: PUA_Variant; out outputSize: size_t; out output: PUA_Variant): UA_StatusCode; cdecl;
  {$ENDIF}

  UA_Client_Subscriptions_create: function(client: PUA_Client;
    const request: UA_CreateSubscriptionRequest; subscriptionContext: Pointer;
    statusChangeCallback: UA_Client_StatusChangeNotificationCallback;
    deleteCallback: UA_Client_DeleteSubscriptionCallback): UA_CreateSubscriptionResponse; cdecl;
  UA_Client_Subscriptions_delete: function(client: PUA_Client; const request: UA_DeleteSubscriptionsRequest): UA_DeleteSubscriptionsResponse; cdecl;
  UA_Client_Subscriptions_deleteSingle: function(client: PUA_Client; subscriptionId: UA_UInt32): UA_StatusCode; cdecl;
  UA_Client_MonitoredItems_createDataChange: function(client: PUA_Client; subscriptionId: UA_UInt32;
    timestampsToReturn: UA_TimestampsToReturn; const item: UA_MonitoredItemCreateRequest;
    context: Pointer; callback: UA_Client_DataChangeNotificationCallback;
    deleteCallback: UA_Client_DeleteMonitoredItemCallback): UA_MonitoredItemCreateResult; cdecl;
  UA_Client_MonitoredItems_deleteSingle: function(client: PUA_Client; subscriptionId: UA_UInt32; monitoredItemId: UA_UInt32): UA_StatusCode; cdecl;

  {$IFDEF ENABLE_SERVER}
  UA_Server_new: function(): PUA_Server; cdecl;
  UA_ServerConfig_setMinimalCustomBuffer: function(config: PUA_ServerConfig;
    portNumber: UA_UInt16; const certificate: PUA_ByteString;
    sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl;
  UA_Server_delete: procedure(server: PUA_Server); cdecl;
  UA_Server_getConfig: function(server: PUA_Server): PUA_ServerConfig; cdecl;
  UA_Server_run: function(server: PUA_Server; running: PUA_Boolean): UA_StatusCode; cdecl;
  UA_Server_run_startup: function(server: PUA_Server): UA_StatusCode; cdecl;
  UA_Server_run_iterate: function(server: PUA_Server; waitInternal: UA_Boolean): UA_UInt16; cdecl;
  UA_Server_run_shutdown: function(server: PUA_Server): UA_StatusCode; cdecl;
  __UA_Server_addNode: function(server: PUA_Server; const nodeClass: UA_NodeClass;
    const requestedNewNodeId: PUA_NodeId;
    const parentNodeId: PUA_NodeId;
    const referenceTypeId: PUA_NodeId;
    const browseName: UA_QualifiedName;
    const typeDefinition: PUA_NodeId;
    const attr: PUA_NodeAttributes;
    const attributeType: PUA_DataType;
    nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;
  __UA_Server_write: function (server: PUA_Server; const nodeId: PUA_NodeId;
    const attributeId: UA_AttributeId; const attr_type: PUA_DataType;
    attr: Pointer): UA_StatusCode; cdecl;
  UA_Server_addReference: function (server: PUA_Server; const sourceId: UA_NodeId;
    const refTypeId: UA_NodeId;
    const targetId: UA_ExpandedNodeId; isForward: UA_Boolean): UA_StatusCode; cdecl;
  UA_Server_deleteReference: function (server: PUA_Server; const sourceNodeId: UA_NodeId;
    const referenceTypeId: UA_NodeId; isForward: UA_Boolean;
    const targetNodeId: UA_ExpandedNodeId; deleteBitirectional: UA_Boolean): UA_StatusCode; cdecl;
  UA_Server_addNamespace: function (server: PUA_Server; namespace: PChar): UA_Uint16; cdecl;
  UA_Server_addMethodNodeEx: function(server: PUA_Server; const requestedNewNodeId: UA_NodeId;
    const parentNodeId: UA_NodeId;
    const referenceTypeId: UA_NodeId;
    const browseName: UA_QualifiedName ;
    const attr: UA_MethodAttributes; method: UA_MethodCallback;
    inputArgumentsSize: size_t; const inputArguments: PUA_Argument;
    const inputArgumentsRequestedNewNodeId: UA_NodeId;
    inputArgumentsOutNewNodeId: PUA_NodeId;
    outputArgumentsSize: size_t; const outputArguments: PUA_Argument;
    const outputArgumentsRequestedNewNodeId: UA_NodeId;
    outputArgumentsOutNewNodeId: PUA_NodeId;
    nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;
  UA_MethodAttributes_default: UA_MethodAttributes;
  {$ENDIF}

  procedure LoadOpen62541();
  procedure UnloadOpen62541();

{$ELSE}

var
  UA_TYPES: array[0..UA_TYPES_COUNT-1] of UA_DataType; external libopen62541;
  UA_VariableAttributes_default: UA_VariableAttributes; external libopen62541;
  UA_ObjectAttributes_default: UA_ObjectAttributes; external libopen62541;
  UA_MethodAttributes_default: UA_MethodAttributes; external libopen62541;
  UA_ObjectTypeAttributes_default: UA_ObjectTypeAttributes; external libopen62541;
  UA_ReferenceTypeAttributes_default: UA_ReferenceTypeAttributes; external libopen62541;
  UA_DataTypeAttributes_default: UA_DataTypeAttributes; external libopen62541;
{$ENDIF}


{$IFNDEF LOAD_DYNAMICALLY}
// Returns the human-readable name of the StatusCode. If no matching StatusCode
// is found, a default string for "Unknown" is returned. This feature might be
// disabled to create a smaller binary with the
// UA_ENABLE_STATUSCODE_DESCRIPTIONS build-flag. Then the function returns an
// empty string for every StatusCode. *)
function UA_StatusCode_name(code: UA_StatusCode): PAnsiChar; cdecl; external libopen62541;

// Copies the content on the heap. Returns a null-string when alloc fails
function UA_String_fromChars(src: PAnsiChar): UA_String; cdecl; external libopen62541;
// Check for equality
function UA_String_equal(const s1, s2: PUA_String): UA_Boolean; cdecl; external libopen62541;
function UA_NodeId_isNull(p: PUA_NodeId): UA_Boolean; cdecl; external libopen62541;
// Print the NodeId in the human-readable format
function UA_NodeId_print(id: PUA_NodeId; output: PUA_String): UA_StatusCode; cdecl; external libopen62541;
function UA_NumericRange_parse(range: PUA_NumericRange; const str: UA_String): UA_StatusCode; cdecl; external libopen62541;
// Set the variant to a scalar value that already resides in memory.
//The value takes on the lifecycle of the variant and is deleted with it.
procedure UA_Variant_setScalar(v: PUA_Variant; p: Pointer; _type: PUA_DataType); cdecl; external libopen62541;
// Set the variant to a scalar value that is copied from an existing variable.
function UA_Variant_setScalarCopy(v: PUA_Variant; p: Pointer; _type: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
// Set the variant to an array that already resides in memory. The array takes on the lifecycle of the variant and is deleted with it.
procedure UA_Variant_setArray(v: PUA_Variant; arrayData: Pointer; arraySize: size_t; _type: PUA_DataType); cdecl; external libopen62541;
// Set the variant to an array that is copied from an existing array
function UA_Variant_setArrayCopy(v: PUA_Variant; arrayData: Pointer;
  arraySize: size_t; _type: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
// Insert a range of data into an existing variant. The data array can't be reused
// afterwards if it contains types without a fixed size (e.g. strings)
// since the members are moved into the variant and take on its lifecycle
function UA_Variant_setRange(v: PUA_Variant; arrayData: Pointer;
  arraySize: size_t; const range: UA_NumericRange): UA_StatusCode; cdecl; external libopen62541;
// Deep-copy a range of data into an existing variant.
function UA_Variant_setRangeCopy(v: PUA_Variant; arrayData: Pointer;
  arraySize: size_t; const range: UA_NumericRange): UA_StatusCode; cdecl; external libopen62541;

function UA_DateTime_toStruct(t: UA_DateTime): UA_DateTimeStruct; cdecl; external libopen62541;
function UA_DateTime_fromStruct(ts: UA_DateTimeStruct): UA_DateTime; cdecl; external libopen62541;

// Returns the data type description for the type's identifier or NULL if no matching data type was found
function UA_findDataType(typeId: PUA_NodeId): PUA_DataType; cdecl; external libopen62541;

// Allocates and initializes a variable of type dataType
function UA_new(const _type: PUA_DataType): Pointer; cdecl; external libopen62541;
// Copies the content of two variables
function UA_copy(src,dst: Pointer; const _type: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
// Deletes the dynamically allocated content of a variable (e.g. resets all
// arrays to undefined arrays). Afterwards, the variable can be safely deleted
// without causing memory leaks. But the variable is not initialized and may
// contain old data that is not memory-relevant
procedure UA_clear(p: Pointer; const _type: PUA_DataType); cdecl; external libopen62541;
// Frees a variable and all of its content
procedure UA_delete(p: Pointer; const _type: PUA_DataType); cdecl; external libopen62541;
// Deletes an array
procedure UA_Array_delete(p: Pointer; size: size_t; const _type: PUA_DataType); cdecl; external libopen62541;
{$ENDIF}


// the non allocating versions use 'var' to emphasize that not a copy but the
//  original value is used (and it shouldn't be changed)  and to make it
//  impossible to use a property as an actual parameter
function _UA_StatusCode_Name(code: UA_StatusCode): AnsiString;
function _UA_STRING(var chars: AnsiString): UA_String; inline;
function _UA_STRING_ALLOC(const chars: AnsiString): UA_String; inline;
function _UA_BYTESTRING(var chars: AnsiString): UA_ByteString; inline;
function _UA_BYTESTRING_ALLOC(const chars: AnsiString): UA_ByteString; inline;
function _UA_QUALIFIEDNAME(nsIndex: UA_UInt16; var chars: AnsiString): UA_QualifiedName;
function _UA_QUALIFIEDNAME_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_QualifiedName; inline;
function _UA_LOCALIZEDTEXT(var locale, text: AnsiString): UA_LocalizedText;
function _UA_LOCALIZEDTEXT_ALLOC(const locale, text: AnsiString): UA_LocalizedText; inline;
function _UA_NUMERICRANGE(const s: AnsiString): UA_NumericRange;
function _UA_String_equal(const s1: UA_String; const s2: AnsiString):boolean; overload;

// my helper functions
function UA_StringToStr(const s: UA_String): AnsiString;
function UA_LocalizedTextToStr(const t: UA_LocalizedText): AnsiString;
function UA_NodeIdToStr(const id: UA_NodeId): AnsiString;
function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
//function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId;
//  const indexRange: AnsiString; out outValue: UA_Variant): UA_StatusCode; overload;

// Returns True if the variant has no value defined (contains neither an array nor a scalar value)
function UA_Variant_isEmpty(const v: PUA_Variant): Boolean;
// Returns True if the variant contains a scalar value.
// Note that empty variants contain an array of length -1 (undefined)
function UA_Variant_isScalar(const v: PUA_Variant): Boolean;
// Returns True if the variant contains a scalar value of the given type
function UA_Variant_hasScalarType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
// Returns True if the variant contains an array of the given type
function UA_Variant_hasArrayType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;

function UA_Variant_getFloat(var v: UA_Variant): Single;
function UA_Variant_getDouble(var v: UA_Variant): Double;
function UA_Variant_getByte(var v: UA_Variant): Byte;
function UA_Variant_getSmallint(var v: UA_Variant): SmallInt;
function UA_Variant_getInteger(var v: UA_Variant): Integer;
function UA_Variant_getInt64(var v: UA_Variant): Int64;
function UA_Variant_getString(var v: UA_Variant): AnsiString; overload;
function UA_Variant_getString(var v: UA_Variant; arrayIndex: DWord): AnsiString; overload;
procedure UA_Variant_setBoolean(out v: UA_Variant; b: ByteBool);
procedure UA_Variant_setFloat(out v: UA_Variant; f: Single);
procedure UA_Variant_setDouble(out v: UA_Variant; d: Double);
procedure UA_Variant_setByte(out v: UA_Variant; i: Byte);
procedure UA_Variant_setSmallint(out v: UA_Variant; i: SmallInt);
procedure UA_Variant_setUInt16(out v: UA_Variant; i: UInt16);
procedure UA_Variant_setInteger(out v: UA_Variant; i: Integer);
procedure UA_Variant_setUInt32(out v: UA_Variant; i: UInt32);
procedure UA_Variant_setInt64(out v: UA_Variant; i: Int64);
procedure UA_Variant_setUInt64(out v: UA_Variant; i: UInt64);
procedure UA_Variant_setString(out v: UA_Variant; const s: AnsiString);

// The following functions are shorthand for creating NodeIds
function UA_NODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_NodeId;
function UA_NODEID_STRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
function UA_NODEID_STRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
function UA_NODEID_GUID(nsIndex: UA_UInt16; guid: UA_Guid): UA_NodeId;
function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
function UA_NODEID_BYTESTRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
function UA_EXPANDEDNODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_Uint32): UA_ExpandedNodeId;

// Test if the data type is a numeric builtin data type. This includes Boolean,
// integers and floating point numbers. Not included are DateTime and StatusCode. *)
//function UA_DataType_isNumeric(_type: PUA_DataType): UA_Boolean; cdecl; external libopen62541;

// Initializes a variable to default values *)
procedure UA_init(p: Pointer; const _type: PUA_DataType);


implementation


uses
{$IFDEF LOAD_DYNAMICALLY}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  DynLibs,
  {$ENDIF}
{$ENDIF}
  // ---
  open62541_types_generated_handling,
  open62541_ua_types,
  open62541_client_highlevel
  ;


{$IFDEF LOAD_DYNAMICALLY}
var
  {$IFDEF MSWINDOWS}
  open62541LibHandle: THandle;
  {$ELSE}
  open62541LibHandle: TLibHandle;
  {$ENDIF}
  RefCount: Integer;


procedure LoadOpen62541();
begin
  Inc(RefCount);
  if RefCount <> 1 then
    Exit;

  open62541LibHandle := LoadLibrary(libopen62541);
  if open62541LibHandle = 0 then
  begin
    RefCount := 0;
    raise EInOutError.CreateFmt('Can not load library "%s". Check your installation.' + sLineBreak + '%s',
      [libopen62541, GetLoadErrorStr()]);
  end;

  Pointer(UA_TYPES) := GetProcedureAddress(open62541LibHandle,'UA_TYPES'); // external variable name
  UA_VariableAttributes_default := PUA_VariableAttributes(GetProcedureAddress(open62541LibHandle, 'UA_VariableAttributes_default'))^;
  UA_MethodAttributes_default := PUA_MethodAttributes(GetProcedureAddress(open62541LibHandle, 'UA_MethodAttributes_default'))^;
  UA_ObjectAttributes_default := PUA_ObjectAttributes(GetProcedureAddress(open62541LibHandle, 'UA_ObjectAttributes_default'))^;
  UA_ObjectTypeAttributes_default := PUA_ObjectTypeAttributes(GetProcedureAddress(open62541LibHandle, 'UA_ObjectTypeAttributes_default'))^;
  UA_ReferenceTypeAttributes_default := PUA_ReferenceTypeAttributes(GetProcedureAddress(open62541LibHandle, 'UA_ReferenceTypeAttributes_default'))^;
  UA_DataTypeAttributes_default := PUA_DataTypeAttributes(GetProcedureAddress(open62541LibHandle, 'UA_DataTypeAttributes_default'))^;

  @UA_Client_new := GetProcedureAddress(open62541LibHandle, 'UA_Client_new');
  @UA_Client_newWithConfig := GetProcedureAddress(open62541LibHandle, 'UA_Client_newWithConfig');
  @UA_Client_getState := GetProcedureAddress(open62541LibHandle, 'UA_Client_getState');
  @UA_Client_getConfig := GetProcedureAddress(open62541LibHandle, 'UA_Client_getConfig');
  @UA_ClientConfig_setDefault := GetProcedureAddress(open62541LibHandle, 'UA_ClientConfig_setDefault');
  @UA_ClientConfig_setDefaultEncryption := GetProcedureAddress(open62541LibHandle, 'UA_ClientConfig_setDefaultEncryption');
  {$IFDEF UA_VER1_3}
  @UA_CertificateVerification_AcceptAll := GetProcedureAddress(open62541LibHandle,'UA_CertificateVerification_AcceptAll');
  {$ENDIF}
  @UA_Client_delete := GetProcedureAddress(open62541LibHandle, 'UA_Client_delete');
  @UA_StatusCode_name := GetProcedureAddress(open62541LibHandle, 'UA_StatusCode_name');
  @UA_Client_connect := GetProcedureAddress(open62541LibHandle, 'UA_Client_connect');
  @UA_Client_disconnect := GetProcedureAddress(open62541LibHandle, 'UA_Client_disconnect');
  @__UA_Client_Service := GetProcedureAddress(open62541LibHandle, '__UA_Client_Service');
  @UA_Client_run_iterate := GetProcedureAddress(open62541LibHandle, 'UA_Client_run_iterate');

  @UA_String_fromChars := GetProcedureAddress(open62541LibHandle, 'UA_String_fromChars');
  @UA_String_equal := GetProcedureAddress(open62541LibHandle, 'UA_String_equal');
  @UA_NodeId_isNull := GetProcedureAddress(open62541LibHandle, 'UA_NodeId_isNull');
  @UA_NodeId_print := GetProcedureAddress(open62541LibHandle, 'UA_NodeId_print');
  @UA_NumericRange_parse := GetProcedureAddress(open62541LibHandle, 'UA_NumericRange_parse');
  @UA_Variant_setScalar := GetProcedureAddress(open62541LibHandle, 'UA_Variant_setScalar');
  @UA_Variant_setScalarCopy := GetProcedureAddress(open62541LibHandle, 'UA_Variant_setScalarCopy');
  @UA_Variant_setArray := GetProcedureAddress(open62541LibHandle, 'UA_Variant_setArray');
  @UA_Variant_setArrayCopy := GetProcedureAddress(open62541LibHandle, 'UA_Variant_setArrayCopy');
  @UA_DateTime_toStruct := GetProcedureAddress(open62541LibHandle, 'UA_DateTime_toStruct');
  @UA_DateTime_fromStruct := GetProcedureAddress(open62541LibHandle, 'UA_DateTime_fromStruct');
  @UA_findDataType := GetProcedureAddress(open62541LibHandle, 'UA_findDataType');

  @UA_new := GetProcedureAddress(open62541LibHandle, 'UA_new');
  @UA_copy := GetProcedureAddress(open62541LibHandle, 'UA_copy');
  @UA_clear := GetProcedureAddress(open62541LibHandle, 'UA_clear');
  @UA_delete := GetProcedureAddress(open62541LibHandle, 'UA_delete');
  @UA_Array_delete := GetProcedureAddress(open62541LibHandle, 'UA_Array_delete');

  @__UA_Client_readAttribute := GetProcedureAddress(open62541LibHandle, '__UA_Client_readAttribute');
  @UA_Client_readArrayDimensionsAttribute := GetProcedureAddress(open62541LibHandle, 'UA_Client_readArrayDimensionsAttribute');
  @__UA_Client_writeAttribute := GetProcedureAddress(open62541LibHandle, '__UA_Client_writeAttribute');
  @UA_Client_writeArrayDimensionsAttribute := GetProcedureAddress(open62541LibHandle, 'UA_Client_writeArrayDimensionsAttribute');
  @UA_Client_call := GetProcedureAddress(open62541LibHandle, 'UA_Client_call');

  @UA_Client_Subscriptions_create := GetProcedureAddress(open62541LibHandle, 'UA_Client_Subscriptions_create');
  @UA_Client_Subscriptions_delete := GetProcedureAddress(open62541LibHandle, 'UA_Client_Subscriptions_delete');
  @UA_Client_Subscriptions_deleteSingle := GetProcedureAddress(open62541LibHandle, 'UA_Client_Subscriptions_deleteSingle');
  @UA_Client_MonitoredItems_createDataChange := GetProcedureAddress(open62541LibHandle, 'UA_Client_MonitoredItems_createDataChange');
  @UA_Client_MonitoredItems_deleteSingle := GetProcedureAddress(open62541LibHandle, 'UA_Client_MonitoredItems_deleteSingle');

  @UA_Server_new := GetProcedureAddress(open62541LibHandle, 'UA_Server_new');
  @UA_ServerConfig_setMinimalCustomBuffer := GetProcedureAddress(open62541LibHandle, 'UA_ServerConfig_setMinimalCustomBuffer');
  @UA_Server_delete := GetProcedureAddress(open62541LibHandle, 'UA_Server_delete');
  @UA_Server_getConfig := GetProcedureAddress(open62541LibHandle, 'UA_Server_getConfig');
  @UA_Server_run := GetProcedureAddress(open62541LibHandle, 'UA_Server_run');
  @UA_Server_run_startup := GetProcedureAddress(open62541LibHandle, 'UA_Server_run_startup');
  @UA_Server_run_iterate := GetProcedureAddress(open62541LibHandle, 'UA_Server_run_iterate');
  @UA_Server_run_shutdown := GetProcedureAddress(open62541LibHandle, 'UA_Server_run_shutdown');

  @__UA_Server_addNode := GetProcedureAddress(open62541LibHandle, '__UA_Server_addNode');
  @UA_Server_addReference:= GetProcedureAddress(open62541LibHandle, 'UA_Server_addReference');
  @UA_Server_deleteReference:= GetProcedureAddress(open62541LibHandle, 'UA_Server_deleteReference');
  @__UA_Server_write := GetProcedureAddress(open62541LibHandle, '__UA_Server_write');
  @UA_Server_addNamespace := GetProcedureAddress(open62541LibHandle, 'UA_Server_addNamespace');
  @UA_Server_addMethodNodeEx := GetProcedureAddress(open62541LibHandle, 'UA_Server_addMethodNodeEx');
end;


procedure UnloadOpen62541;
begin
  if RefCount > 0 then
    begin
      Dec(RefCount);
      if RefCount = 0 then
        begin
          UnloadLibrary(open62541LibHandle);
        end;
    end;
end;
{$ENDIF}


// ---


function _UA_StatusCode_Name(code: UA_StatusCode): AnsiString;
begin
  Result := Format('%x:%s', [code, AnsiString(UA_StatusCode_name(code))]);
end;


// UA_STRING returns a string pointing to the original char-array.
// UA_STRING_ALLOC is shorthand for UA_String_fromChars and makes a copy
// of the char-array
function _UA_STRING(var chars: AnsiString): UA_String; inline;
begin
  if chars = '' then
    begin
      Result.length := 0;
      Result.data := nil;
    end
  else
    begin
      Result.length := Length(chars);
      Result.data := @chars[1];
    end;
end;


function _UA_STRING_ALLOC(const chars: AnsiString): UA_String; inline;
var
  UABStr: UA_BYTESTRING;
begin
  if chars = '' then
    begin
      Result.length := 0;
      Result.data := nil;
    end
  else
    begin
      // this contortion is necessary in order to let the library allocate the memory
      // (the C and pascal alloc/free cannot be mixed)
      UABStr.length := Length(chars);
      UABStr.data := @chars[1];
      UA_copy(@UABStr, @Result, @UA_TYPES[UA_TYPES_BYTESTRING]);
    end;
end;


function _UA_BYTESTRING(var chars: AnsiString): UA_ByteString;
begin
  Result := UA_ByteString(_UA_STRING(chars));
end;


function _UA_BYTESTRING_ALLOC(const chars: AnsiString): UA_ByteString; inline;
begin
  Result := UA_ByteString(_UA_STRING_ALLOC(chars));
end;


function _UA_QUALIFIEDNAME(nsIndex: UA_UInt16;
  var chars: AnsiString): UA_QualifiedName;
begin
  Result.namespaceIndex := nsIndex;
  Result.name := _UA_STRING(chars);
end;


function _UA_QUALIFIEDNAME_ALLOC(nsIndex: UA_UInt16;
  const chars: AnsiString): UA_QualifiedName;
begin
  Result.namespaceIndex := nsIndex;
  Result.name := _UA_STRING_ALLOC(chars);
end;


function _UA_LOCALIZEDTEXT(var locale, text: AnsiString): UA_LocalizedText;
begin
  Result.locale := _UA_STRING(locale);
  Result.text := _UA_STRING(text);
end;


function _UA_LOCALIZEDTEXT_ALLOC(const locale, text: AnsiString): UA_LocalizedText;
begin
  Result.locale := _UA_STRING_ALLOC(locale);
  Result.text := _UA_STRING_ALLOC(text)
end;


function _UA_NUMERICRANGE(const s: AnsiString): UA_NumericRange;
var
  UAS: UA_String;
begin
  Result.dimensionsSize := 0;
  Result.dimensions := nil;
  UAS :=_UA_STRING_ALLOC(s);
  UA_NumericRange_parse(@Result, UAS);
  UA_String_clear(UAS);
end;


function _UA_String_equal(const s1: UA_String; const s2: AnsiString): Boolean; overload;
var
  UAStr: UA_String;
begin
  UAStr := _UA_STRING_ALLOC(s2);
  Result := UA_String_equal(@s1, @UAStr);
  UA_String_clear(UAStr);
end;


function UA_StringToStr(const s: UA_String): AnsiString;
begin
  SetString(Result, PAnsiChar(s.data), s.length);
end;


function UA_LocalizedTextToStr(const t: UA_LocalizedText): AnsiString;
begin
  SetString(Result, PAnsiChar(t.text.data), t.text.length);
end;


function UA_NodeIdToStr(const id: UA_NodeId): AnsiString;
var
  Output: UA_String;
begin
  Output := UA_STRING_NULL;
  UA_NodeId_print(@id, @Output);
  SetString(Result, PAnsiChar(Output.data), Output.length);
  UA_String_Clear(Output);
end;


function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
var
  PDataType: PUA_DataType;
begin
  PDataType := UA_findDataType(@typeId);
  if PDataType = nil then
    Result := 'Unknown'
  else
    Result := PDataType^.typeName;
end;


// ---


function UA_Variant_isEmpty(const v: PUA_Variant): Boolean;
begin
  Result := v^._type = nil;
end;


function UA_Variant_isScalar(const v: PUA_Variant): Boolean;
begin
  Result := (v^.arrayLength = 0) and (PByte(v^.data) > PByte(UA_EMPTY_ARRAY_SENTINEL));
end;


function UA_Variant_hasScalarType(const v: PUA_Variant;
  const _type: PUA_DataType): Boolean;
begin
  Result := UA_Variant_isScalar(v) and (_type = v^._type);
end;


function UA_Variant_hasArrayType(const v: PUA_Variant;
  const _type: PUA_DataType): Boolean;
begin
  Result := (not UA_Variant_isScalar(v)) and (_type = v^._type);
end;


function UA_Variant_getFloat(var v: UA_Variant): Single;
begin
  Result := PUA_Float(v.data)^;
end;


function UA_Variant_getDouble(var v: UA_Variant): Double;
begin
  Result := PUA_Double(v.data)^;
end;


function UA_Variant_getByte(var v: UA_Variant): Byte;
begin
  Result := PUA_Byte(v.data)^;
end;


function UA_Variant_getSmallint(var v: UA_Variant): SmallInt;
begin
  Result := PUA_Int16(v.data)^;
end;


function UA_Variant_getInteger(var v: UA_Variant): Integer;
begin
  Result := PUA_Int32(v.data)^;
end;


function UA_Variant_getInt64(var v: UA_Variant): Int64;
begin
  Result := PUA_Int64(v.data)^;
end;


function UA_Variant_getString(var v: UA_Variant): AnsiString;
begin
  SetString(Result, PAnsiChar(PUA_String(v.data)^.data), PUA_String(v.data)^.length);
end;


function UA_Variant_getString(var v: UA_Variant;
  arrayIndex: DWord): AnsiString;
begin
  if arrayIndex < v.arrayLength then
    SetString(Result, PAnsiChar(PUA_String(v.data)[arrayIndex].data), PUA_String(v.data)[arrayIndex].length)
  else
    Result := '';
end;


procedure UA_Variant_setBoolean(out v: UA_Variant; b: ByteBool);
begin
  UA_Variant_setScalarCopy(@v, @b, @UA_TYPES[UA_TYPES_BOOLEAN]);
end;


procedure UA_Variant_setFloat(out v: UA_Variant; f: Single);
begin
  UA_Variant_setScalarCopy(@v, @f, @UA_TYPES[UA_TYPES_FLOAT]);
end;


procedure UA_Variant_setDouble(out v: UA_Variant; d: Double);
begin
  UA_Variant_setScalarCopy(@v, @d, @UA_TYPES[UA_TYPES_DOUBLE]);
end;


procedure UA_Variant_setByte(out v: UA_Variant; i: Byte);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_BYTE]);
end;


procedure UA_Variant_setSmallint(out v: UA_Variant; i: SmallInt);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_INT16]);
end;


procedure UA_Variant_setUInt16(out v: UA_Variant; i: UInt16);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_UINT16]);
end;


procedure UA_Variant_setInteger(out v: UA_Variant; i: Integer);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_INT32]);
end;


procedure UA_Variant_setUInt32(out v: UA_Variant; i: UInt32);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_UINT32]);
end;


procedure UA_Variant_setInt64(out v: UA_Variant; i: Int64);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_INT64]);
end;


procedure UA_Variant_setUInt64(out v: UA_Variant; i: UInt64);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_UINT64]);
end;


procedure UA_Variant_setString(out v: UA_Variant; const s: AnsiString);
var
  UAStr: UA_STRING;
begin
  UAStr := _UA_STRING_ALLOC(s);
  UA_Variant_setScalarCopy(@v, @UAStr, @UA_TYPES[UA_TYPES_STRING]);
  UA_String_clear(UAStr);
end;


// ---


function UA_NODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_NUMERIC;
  Result.identifier.numeric := identifier;
end;


function UA_NODEID_STRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_STRING;
  Result.identifier._string := _UA_STRING(chars);
end;


function UA_NODEID_STRING_ALLOC(nsIndex: UA_UInt16;
  const chars: AnsiString): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_STRING;
  Result.identifier._string := _UA_STRING_ALLOC(chars);
end;


function UA_NODEID_GUID(nsIndex: UA_UInt16; guid: UA_Guid): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_GUID;
  Result.identifier.guid := guid;
end;


function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_BYTESTRING;
  Result.identifier.byteString := _UA_BYTESTRING(chars);
end;


function UA_NODEID_BYTESTRING_ALLOC(nsIndex: UA_UInt16;
  const chars: AnsiString): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_BYTESTRING;
  Result.identifier.byteString := _UA_BYTESTRING_ALLOC(chars);
end;


function UA_EXPANDEDNODEID_NUMERIC(nsIndex: UA_UInt16;
  identifier: UA_Uint32): UA_ExpandedNodeId;
begin
  Result.nodeId:=UA_NODEID_NUMERIC(nsIndex, identifier);
  Result.serverIndex:=0;
  Result.namespaceUri:=UA_STRING_NULL;
end;


procedure UA_init(p: Pointer; const _type: PUA_DataType);
begin
  FillChar(p^, _type^.memSize, #0);
end;


end.

