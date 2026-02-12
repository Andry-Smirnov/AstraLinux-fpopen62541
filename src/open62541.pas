{$CODEPAGE UTF8}
(*
  open62541 is licensed under the Mozilla Public License v2.0 (MPLv2).
  This allows the open62541 library to be combined and distributed with any proprietary software.
  Only changes to the open62541 library itself need to be licensed under the MPLv2 when copied and distributed.
  The plugins, as well as the server and client examples are in the public domain (CC0 license).
  They can be reused under any license and changes do not have to be published.
 
  Version 1.2-rc2 released on 23 Dec 2020
 
  BEWARE: between version 1.1 and version 1.2 many structures
  and ids have changed so the two versions are not binary compatible
  (i.e you cannot use these headers with version 1.1)
 
  Author: Lacak <lacak At Sourceforge>
 
  Contributors: 
    Luca Olivetti <luca@ventoso.org>


  Example:
    var
      Client: PUA_Client;
      Config: PUA_ClientConfig;
      NodeId: UA_NodeId;
      Value: UA_Variant;
    begin
      Client := UA_Client_new();
      Config := UA_Client_getConfig(Client);
      UA_ClientConfig_setDefault(Config);
      if UA_Client_connect(Client, 'opc.tcp://localhost...') = UA_STATUSCODE_GOOD then
      begin
        NodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME);
        UA_Variant_init(Value);
        if (UA_Client_readValueAttribute(Client, NodeId, Value) = UA_STATUSCODE_GOOD) and
           (UA_Variant_hasScalarType(@Value, @UA_TYPES[UA_TYPES_DATETIME])) then
        begin
           ...
        end;
        UA_Variant_clear(Value);
      end;
      UA_Client_delete(Client);
    end;
 *)
unit open62541;


{$I open62541_definitions.inc}

(*
{$IFDEF FPC}
  {$IFDEF LOAD_DYNAMICALLY}
    {$MODE DELPHI}
    {$HINT DELPHI MODE SELECTED}
  {$ENDIF}
{$ENDIF}
{$H+}
{$POINTERMATH ON}
*)

interface


uses
  open62541_types,
  open62541_types_generated,
  open62541_common,
  open62541_network,
  open62541_server,
  open62541_server_config,
  open62541_securitypolicy
{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
  ,
  open62541_client_subscriptions
{$ENDIF}
{$IFNDEF LOAD_DYNAMICALLY}
  ,
  open62541_client,
  open62541_client_highlevel,
{$ENDIF}
  ;


const
// Library version
{$IFDEF UA_VER1_5}
  UA_VER = 1.5;
{$ENDIF}
{$IFDEF UA_VER1_4}
  UA_VER = 1.4;
{$ENDIF}
{$IFDEF UA_VER1_3}
  UA_VER = 1.3;
{$ENDIF}
{$IFDEF UA_VER1_2}
  UA_VER = 1.2;
{$ENDIF}


type

  // --- plugin/pki.h ---

  PUA_CertificateVerification  = ^UA_CertificateVerification;
  UA_CertificateVerification = record
    context: Pointer;
    verifyCertificate: function (verificationContext: Pointer;
      certificate: PUA_ByteString): UA_StatusCode; cdecl;
    verifyApplicationURI: function (verificationContext: Pointer;
      certificate: PUA_ByteString; applicationURI: PUA_String): UA_StatusCode; cdecl;
    deleteMembers: procedure (cv:PUA_CertificateVerification); cdecl;
  end;


  // --- client_config.h ---

  UA_ClientConfig = record
    // Basic client configuration

    // User-defined data attached to the client
    clientContext: Pointer;
    // Logger used by the client
    logger: UA_Logger;
    // Response timeout in ms
    timeout: UA_UInt32;

    // The description must be internally consistent.
    // - The ApplicationUri set in the ApplicationDescription must match the
    //   URI set in the server certificate
    clientDescription: UA_ApplicationDescription;

    // Basic connection configuration

    // Configured User-Identity Token
    userIdentityToken: UA_ExtensionObject;
    // None, Sign, SignAndEncrypt. The default is invalid. This indicates
    // the client to select any matching endpoint
    securityMode: UA_MessageSecurityMode;
    // SecurityPolicy for the SecureChannel. An empty string indicates
    // the client to select any matching SecurityPolicy. *)
    securityPolicyUri: UA_String;

    // Advanced connection configuration

    // If either endpoint or userTokenPolicy has been set (at least one non-zero
    // byte in either structure), then the selected Endpoint and UserTokenPolicy
    // overwrite the settings in the basic connection configuration.
    // The userTokenPolicy array in the EndpointDescription is ignored.
    // The selected userTokenPolicy is set in the dedicated configuration field.
    //
    // If the advanced configuration is not set, the client will write to it the
    // selected Endpoint and UserTokenPolicy during GetEndpoints.
    //
    // The information in the advanced configuration is used during reconnect
    // when the SecureChannel was broken.
    endpoint: UA_EndpointDescription;
    userTokenPolicy: UA_UserTokenPolicy;

    // Advanced client configuration

    // Lifetime in ms (then the channel needs to be renewed)
    secureChannelLifeTime: UA_UInt32;
    // Session timeout in ms
    requestedSessionTimeout: UA_UInt32;
    localConnectionConfig: UA_ConnectionConfig;
    // Connectivity check interval in ms. 0 = background task disabled
    connectivityCheckInterval: UA_UInt32 ;
    // Custom DataTypes. Attention! Custom datatypes are not cleaned
    // up together with the configuration. So it is possible
    // to allocate them on ROM
    customDataTypes: ^UA_DataTypeArray;

    // Available SecurityPolicies
    securityPoliciesSize: size_t;
    securityPolicies: ^UA_SecurityPolicy;

    // Certificate Verification Plugin
    certificateVerification: UA_CertificateVerification;

    // Callbacks for async connection handshakes
    initConnectionFunc: UA_ConnectClientConnection;
    pollConnectionFunc: function(
      client: PUA_Client; 
      context: Pointer; 
      timeout: UA_UInt32): UA_StatusCode; cdecl;

    // Callback for state changes. The client state is differentated into the
    // SecureChannel state and the Session state. The connectStatus is set if
    // the client connection (including reconnects) has failed and the client
    // has to "give up". If the connectStatus is not set, the client still has
    // hope to connect or recover.
    stateCallback: procedure (
      client: PUA_Client;
      channelState: UA_SecureChannelState;
      sessionState: UA_SessionState;
      connectStatus: UA_StatusCode); cdecl;

    // When connectivityCheckInterval is greater than 0, every
    // connectivityCheckInterval (in ms), a async read request is performed on
    // the server. inactivityCallback is called when the client receive no
    // response for this read request The connection can be closed, this in an
    // attempt to recreate a healthy connection. *)
    inactivityCallback: procedure (client: PUA_Client); cdecl;

    {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
    // Number of PublishResponse queued up in the server
    outStandingPublishRequests: UA_UInt16;

    // If the client does not receive a PublishResponse after the defined delay
    // of ``(sub->publishingInterval * sub->maxKeepAliveCount) +
    // client->config.timeout)``, then subscriptionInactivityCallback is called
    // for the subscription.. *)
    subscriptionInactivityCallback: procedure(
      client: PUA_Client; 
      subscriptionId: UA_UInt32; 
      subContext: Pointer); cdecl;
    {$ENDIF}
  end;
  PUA_ClientConfig = ^UA_ClientConfig;


const

  // --- common.h ---

  (**
   * Access Level Masks
   * ------------------
   * The access level to a node is given by the following constants that are ANDed
   * with the overall access level. *)
  UA_ACCESSLEVELMASK_READ           = $01 shl 0;
  UA_ACCESSLEVELMASK_WRITE          = $01 shl 1;
  UA_ACCESSLEVELMASK_HISTORYREAD    = $01 shl 2;
  UA_ACCESSLEVELMASK_HISTORYWRITE   = $01 shl 3;
  UA_ACCESSLEVELMASK_SEMANTICCHANGE = $01 shl 4;
  UA_ACCESSLEVELMASK_STATUSWRITE    = $01 shl 5;
  UA_ACCESSLEVELMASK_TIMESTAMPWRITE = $01 shl 6;


  // --- statuscodes.h ---
{$IFDEF UA_VER1_5}
  {$I const/statuscodes_1_5.inc}
{$ENDIF}
{$IFDEF UA_VER1_4}
  {$I const/statuscodes_1_4.inc}
{$ENDIF}
{$IFDEF UA_VER1_3}
  {$I const/statuscodes_1_3.inc}
{$ENDIF}
{$IFDEF UA_VER1_2}
  {$I const/statuscodes_1_2.inc}
{$ENDIF}


  // --- nodeids.h ---
{$IFDEF UA_VER1_5}
  {$I const/nodeids_1_5.inc}
{$ENDIF}
{$IFDEF UA_VER1_4}
  {$I const/nodeids_1_4.inc}
{$ENDIF}
{$IFDEF UA_VER1_3}
  {$I const/nodeids_1_3.inc}
{$ENDIF}
{$IFDEF UA_VER1_2}
  {$I const/nodeids_1_2.inc}
{$ENDIF}


  // --- ua_types.c ---

  UA_STRING_NULL: UA_String = (length: 0; data: nil);
  UA_BYTESTRING_NULL: UA_ByteString = (length: 0; data: nil);
  UA_GUID_NULL: UA_Guid = (
    data1: 0;
    data2: 0;
    data3: 0;
    data4: (0, 0, 0, 0, 0, 0, 0, 0)
  );
  UA_NODEID_NULL: UA_NodeId = (
    namespaceIndex: 0;
    identifierType: UA_NodeIdTYPE_NUMERIC;
    identifier: (numeric: 0)
  );


{$IFDEF LOAD_DYNAMICALLY}
var
  UA_TYPES: PUA_DataType;
  UA_VariableAttributes_default: UA_VariableAttributes;
  UA_ObjectAttributes_default: UA_ObjectAttributes;
  UA_ObjectTypeAttributes_default: UA_ObjectTypeAttributes;
  UA_ReferenceTypeAttributes_default: UA_ReferenceTypeAttributes;
  UA_DataTypeAttributes_default: UA_DataTypeAttributes;

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

  __UA_Client_readAttribute: function(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl;
  UA_Client_readArrayDimensionsAttribute: function(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: size_t; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl;

  __UA_Client_writeAttribute: function(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl;
  UA_Client_writeArrayDimensionsAttribute: function(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: size_t; newArrayDimensions: PUA_UInt32): UA_StatusCode;
  {$IFDEF UA_ENABLE_METHODCALLS}
  UA_Client_call: function(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: size_t; input: PUA_Variant; out outputSize: size_t; out output: PUA_Variant): UA_StatusCode; cdecl;
  {$ENDIF}

  UA_Client_Subscriptions_create: function(client: PUA_Client;
    const request: UA_CreateSubscriptionRequest; subscriptionContext: Pointer; statusChangeCallback: UA_Client_StatusChangeNotificationCallback; deleteCallback: UA_Client_DeleteSubscriptionCallback): UA_CreateSubscriptionResponse; cdecl;
  UA_Client_Subscriptions_delete: function(client: PUA_Client; const request: UA_DeleteSubscriptionsRequest): UA_DeleteSubscriptionsResponse; cdecl;
  UA_Client_Subscriptions_deleteSingle: function(client: PUA_Client; subscriptionId: UA_UInt32): UA_StatusCode; cdecl;
  UA_Client_MonitoredItems_createDataChange: function(client: PUA_Client; subscriptionId: UA_UInt32; timestampsToReturn: UA_TimestampsToReturn; const item: UA_MonitoredItemCreateRequest; context: Pointer; callback: UA_Client_DataChangeNotificationCallback; deleteCallback: UA_Client_DeleteMonitoredItemCallback): UA_MonitoredItemCreateResult; cdecl;
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


{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
// --- client_subscriptions.h ---

// Subscriptions (.. _client-subscriptions:)
//
// Subscriptions in OPC UA are asynchronous. That is, the client sends several
// PublishRequests to the server. The server returns PublishResponses with
// notifications. But only when a notification has been generated. The client
// does not wait for the responses and continues normal operations.
//
// Note the difference between Subscriptions and MonitoredItems.
// Subscriptions are used to report back notifications.
// MonitoredItems are used to generate notifications.
// Every MonitoredItem is attached to exactly one Subscription.
// And a Subscription can contain many MonitoredItems.
//
// The client automatically processes PublishResponses (with a callback) in the
// background and keeps enough PublishRequests in transit. The PublishResponses
// may be recieved during a synchronous service call or in
// ``UA_Client_runAsync``. *)
function UA_Client_Subscriptions_create(client: PUA_Client;
  const request: UA_CreateSubscriptionRequest; subscriptionContext: Pointer;
  statusChangeCallback: UA_Client_StatusChangeNotificationCallback;
  deleteCallback: UA_Client_DeleteSubscriptionCallback): UA_CreateSubscriptionResponse; cdecl; external libopen62541;
function UA_Client_Subscriptions_delete(client: PUA_Client;
  const request: UA_DeleteSubscriptionsRequest): UA_DeleteSubscriptionsResponse; cdecl; external libopen62541;
// Delete a Single subscription
function UA_Client_Subscriptions_deleteSingle(client: PUA_Client;
  subscriptionId: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;

// MonitoredItems
//
// MonitoredItems for Events indicate the ``EventNotifier`` attribute. This
// indicates to the server not to monitor changes of the attribute, but to
// forward Event notifications from that node.
//
// During the creation of a MonitoredItem, the server may return changed
// adjusted parameters. Check the returned ``UA_CreateMonitoredItemsResponse``
// to get the current parameters.
//
// Clients define MonitoredItems to subscribe to data and Events.
// Each MonitoredItem identifies the item to be monitored and the Subscription are used to report Notifications to the Client.
// The item to be monitored may be any Node Attribute.

// Don't use to monitor the EventNotifier attribute
function UA_Client_MonitoredItems_createDataChanges(client: PUA_Client;
  const request: UA_CreateMonitoredItemsRequest; contexts: PPointer;
  callbacks: UA_Client_DataChangeNotificationCallback;
  deleteCallbacks: UA_Client_DeleteMonitoredItemCallback): UA_CreateMonitoredItemsResponse; cdecl; external libopen62541;
function UA_Client_MonitoredItems_createDataChange(client: PUA_Client;
  subscriptionId: UA_UInt32; timestampsToReturn: UA_TimestampsToReturn;
  const item: UA_MonitoredItemCreateRequest; context: Pointer;
  callback: UA_Client_DataChangeNotificationCallback;
  deleteCallback: UA_Client_DeleteMonitoredItemCallback): UA_MonitoredItemCreateResult; cdecl; external libopen62541;
function UA_Client_MonitoredItems_deleteSingle(client: PUA_Client;
  subscriptionId: UA_UInt32; monitoredItemId: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}


{$IFDEF ENABLE_SERVER}
// --- server_config_default.h ---

// Create a new server with default plugins for logging etc. used during
// initialization. No network layer and SecurityPolicies are set so far
function UA_Server_new(): PUA_Server; cdecl; external libopen62541;
function UA_ServerConfig_setMinimalCustomBuffer(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString; sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;


// --- server.h ---

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


{ --- types.h --- }

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
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; 
  const indexRange: AnsiString; out outValue: UA_Variant): UA_StatusCode; overload;

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


// --- types_generated_handling.h ---

procedure UA_Variant_init(out p: UA_Variant);
procedure UA_Variant_clear(var p: UA_Variant);
procedure UA_String_clear(var p: UA_String);
procedure UA_NodeId_clear(var p: UA_NodeId);
procedure UA_CreateSubscriptionRequest_init(out p: UA_CreateSubscriptionRequest);
procedure UA_MonitoredItemCreateRequest_init(out p: UA_MonitoredItemCreateRequest);

procedure UA_BrowseRequest_init(out p: UA_BrowseRequest);
function UA_BrowseRequest_new:PUA_BrowseRequest;
function UA_BrowseRequest_copy(const src: UA_BrowseRequest; out dst:UA_BrowseRequest): UA_StatusCode;
procedure UA_BrowseRequest_deleteMembers(var p: UA_BrowseRequest);
procedure UA_BrowseRequest_clear(var p: UA_BrowseRequest);
procedure UA_BrowseRequest_delete(p: PUA_BrowseRequest);

procedure UA_BrowseResponse_init(out p: UA_BrowseResponse);
function UA_BrowseResponse_new:PUA_BrowseResponse;
function UA_BrowseResponse_copy(const src: UA_BrowseResponse; out dst:UA_BrowseResponse): UA_StatusCode;
procedure UA_BrowseResponse_deleteMembers(var p: UA_BrowseResponse);
procedure UA_BrowseResponse_clear(var p: UA_BrowseResponse);
procedure UA_BrowseResponse_delete(p: PUA_BrowseResponse);

procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
function UA_BrowseDescription_new:PUA_BrowseDescription;
function UA_BrowseDescription_copy(const src: UA_BrowseDescription; out dst:UA_BrowseDescription): UA_StatusCode;
procedure UA_BrowseDescription_deleteMembers(var p: UA_BrowseDescription);
procedure UA_BrowseDescription_clear(var p: UA_BrowseDescription);
procedure UA_BrowseDescription_delete(p: PUA_BrowseDescription);


// --- client.h ---

function UA_Client_connect_username(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode; deprecated;
function UA_Client_connectUsername(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode;
function UA_Client_Service_read(client: PUA_Client;
  const request: UA_ReadRequest): UA_ReadResponse;
function UA_Client_Service_browse(client: PUA_Client;
  const request: UA_BrowseRequest): UA_BrowseResponse;


// --- client_highlevel.h ---

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

{$IFDEF ENABLE_SERVER}
// --- server_config_default.h ---

function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig;
  portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;


// --- server.h ---

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


{$IFDEF LOAD_DYNAMICALLY}
uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  DynLibs,
{$ENDIF}
  open62541_const
  ;

var
{$IFDEF MSWINDOWS}
  open62541LibHandle: THandle;
{$ELSE}
  open62541LibHandle: TLibHandle;
{$ENDIF}
  RefCount: Integer;

{$IFDEF MSWINDOWS}
function GetProcedureAddress(Lib: THandle; ProcName: PAnsiChar): Pointer; inline;
begin
  Result := Windows.GetProcAddress(Lib, ProcName);
end;

function UnloadLibrary(Lib: THandle): Boolean; inline;
begin
  Result := Windows.FreeLibrary(Lib);
end;

function GetLoadErrorStr: string; inline;
begin
  Result := SysErrorMessage(GetLastError);
end;
{$ENDIF}

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

{$ELSE}
uses
  SysUtils;
{$ENDIF}

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

{$IFDEF FPC} {$WARN 5058 OFF : Variable "$1" does not seem to be initialized} {$ENDIF}
procedure UA_Variant_init(out p: UA_Variant);
begin
  FillChar(p, SizeOf(UA_Variant), #0);
end;
{$IFDEF FPC} {$WARN 5058 ON : Variable "$1" does not seem to be initialized} {$ENDIF}

procedure UA_Variant_clear(var p: UA_Variant);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_VARIANT]);
end;

procedure UA_String_clear(var p: UA_String);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_STRING]);
end;

procedure UA_NodeId_clear(var p: UA_NodeId);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_NODEID]);
end;

{$IFDEF FPC} {$WARN 5058 OFF : Variable "$1" does not seem to be initialized} {$ENDIF}
procedure UA_CreateSubscriptionRequest_init(out p: UA_CreateSubscriptionRequest);
begin
  FillChar(p, SizeOf(p), #0);
end;

procedure UA_MonitoredItemCreateRequest_init(out p: UA_MonitoredItemCreateRequest);
begin
  FillChar(p, SizeOf(p), #0);
end;

procedure UA_BrowseRequest_init(out p: UA_BrowseRequest);
begin
  FillChar(p, SizeOf(p), #0);
end;
{$IFDEF FPC} {$WARN 5058 ON : Variable "$1" does not seem to be initialized} {$ENDIF}

function UA_BrowseRequest_new: PUA_BrowseRequest;
begin
  Result := UA_new(@UA_TYPES[UA_TYPES_BROWSEREQUEST]);
end;

function UA_BrowseRequest_copy(const src: UA_BrowseRequest; out
  dst: UA_BrowseRequest): UA_StatusCode;
begin
  Result := UA_copy(@src, @dst, @UA_TYPES[UA_TYPES_BROWSEREQUEST]);
end;

procedure UA_BrowseRequest_deleteMembers(var p: UA_BrowseRequest);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSEREQUEST]);
end;

procedure UA_BrowseRequest_clear(var p: UA_BrowseRequest);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSEREQUEST]);
end;

procedure UA_BrowseRequest_delete(p: PUA_BrowseRequest);
begin
  UA_delete(p, @UA_TYPES[UA_TYPES_BROWSEREQUEST]);
end;

{$IFDEF FPC} {$WARN 5058 OFF : Variable "$1" does not seem to be initialized} {$ENDIF}
procedure UA_BrowseResponse_init(out p: UA_BrowseResponse);
begin
  FillChar(p, SizeOf(p), #0);
end;
{$IFDEF FPC} {$WARN 5058 ON : Variable "$1" does not seem to be initialized} {$ENDIF}

function UA_BrowseResponse_new: PUA_BrowseResponse;
begin
  Result := UA_new(@UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

function UA_BrowseResponse_copy(const src: UA_BrowseResponse; out
  dst: UA_BrowseResponse): UA_StatusCode;
begin
  Result := UA_copy(@src, @dst, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

procedure UA_BrowseResponse_deleteMembers(var p: UA_BrowseResponse);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

procedure UA_BrowseResponse_clear(var p: UA_BrowseResponse);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

procedure UA_BrowseResponse_delete(p: PUA_BrowseResponse);
begin
  UA_delete(p, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

{$IFDEF FPC} {$WARN 5058 OFF : Variable "$1" does not seem to be initialized} {$ENDIF}
procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
begin
  FillChar(p, SizeOf(p), #0);
end;
{$IFDEF FPC} {$WARN 5058 ON : Variable "$1" does not seem to be initialized} {$ENDIF}

function UA_BrowseDescription_new: PUA_BrowseDescription;
begin
  Result := UA_new(@UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
end;

function UA_BrowseDescription_copy(const src: UA_BrowseDescription; out
  dst: UA_BrowseDescription): UA_StatusCode;
begin
  Result := UA_copy(@src, @dst, @UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
end;

procedure UA_BrowseDescription_deleteMembers(var p: UA_BrowseDescription);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
end;

procedure UA_BrowseDescription_clear(var p: UA_BrowseDescription);
begin
  UA_clear(@p, @UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
end;

procedure UA_BrowseDescription_delete(p: PUA_BrowseDescription);
begin
  UA_delete(p, @UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
end;

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

procedure UA_Variant_setString(out v: UA_Variant;
  const s: AnsiString);
var 
  UAStr: UA_STRING;
begin
  UAStr := _UA_STRING_ALLOC(s);
  UA_Variant_setScalarCopy(@v, @UAStr, @UA_TYPES[UA_TYPES_STRING]);
  UA_String_clear(UAStr);
end;

function UA_NODEID_NUMERIC(nsIndex: UA_UInt16;
  identifier: UA_UInt32): UA_NodeId;
begin
  Result.namespaceIndex := nsIndex;
  Result.identifierType := UA_NODEIDTYPE_NUMERIC;
  Result.identifier.numeric := identifier;
end;

function UA_NODEID_STRING(nsIndex: UA_UInt16;
  var chars: AnsiString): UA_NodeId;
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

function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16;
  var chars: AnsiString): UA_NodeId;
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

function UA_Client_connect_username(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode;
begin
  Result := UA_Client_connectUsername(client, endpointUrl, username, password);
end;

// Connect to the server and create+activate a Session with the given username
// and password. This first set the UserIdentityToken in the client config and
// then calls the regular connect method.
function UA_Client_connectUsername(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode;
var
  IdentityToken: PUA_UserNameIdentityToken;
  ClientConfig: PUA_ClientConfig;
begin
  //UA_UserNameIdentityToken_new()
  IdentityToken := PUA_UserNameIdentityToken(UA_new(@UA_TYPES[UA_TYPES_USERNAMEIDENTITYTOKEN]));
  if IdentityToken = nil then
    Result := UA_STATUSCODE_BADOUTOFMEMORY
  else
    begin
      IdentityToken^.userName := _UA_STRING_ALLOC(username);
      IdentityToken^.password := _UA_STRING_ALLOC(password);
      ClientConfig := UA_Client_getConfig(client);
      //UA_ExtensionObject_clear()
      UA_clear(@ClientConfig^.userIdentityToken, @UA_TYPES[UA_TYPES_EXTENSIONOBJECT]);
      ClientConfig^.userIdentityToken.encoding := UA_EXTENSIONOBJECT_DECODED;
      ClientConfig^.userIdentityToken.content.decoded._type := @UA_TYPES[UA_TYPES_USERNAMEIDENTITYTOKEN];
      ClientConfig^.userIdentityToken.content.decoded.data := IdentityToken;
      Result := UA_Client_connect(client, endpointUrl);
    end;
end;

function UA_Client_Service_read(client: PUA_Client;
  const request: UA_ReadRequest): UA_ReadResponse;
begin
  __UA_Client_Service(client, @request, @UA_TYPES[UA_TYPES_READREQUEST], @Result,
    @UA_TYPES[UA_TYPES_READRESPONSE]);
end;

function UA_Client_Service_browse(client: PUA_Client;
  const request: UA_BrowseRequest): UA_BrowseResponse;
begin
  __UA_Client_Service(client, @request, @UA_TYPES[UA_TYPES_BROWSEREQUEST],
    @Result, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

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
  out outValue: Byte): UA_StatusCode;
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
  out outValue: SmallInt): UA_StatusCode;
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
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newValue: UA_Variant): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @newValue, @UA_TYPES[UA_TYPES_VARIANT]);
end;

function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: Byte): UA_StatusCode;
var
  UAVar: UA_Variant;
begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_BYTE]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;

function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: SmallInt): UA_StatusCode;
var
  UAVar: UA_Variant;
begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_INT16]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;

function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: Longint): UA_StatusCode;
var
  UAVar: UA_Variant;
begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_INT32]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;

function UA_Client_writeValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  const newValue: AnsiString): UA_StatusCode;
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
  const newValues: array of AnsiString): UA_StatusCode;
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

{$IFDEF ENABLE_SERVER}
// Creates a server config on the default port 4840 with no server certificate.
function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimal(config, 4840, nil);
end;

(* Creates a new server config with one endpoint.
 *
 * The config will set the tcp network layer to the given port and adds a Single
 * endpoint with the security policy ``SecurityPolicy#None`` to the server. A
 * server certificate may be supplied but is optional. *)
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig;
  portNumber: UA_UInt16;
  const certificate: PUA_ByteString): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimalCustomBuffer(config, portNumber, certificate, 0, 0);
end;

function UA_Server_addVariableNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: UA_NodeId;
  const attr: UA_VariableAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := __UA_Server_addNode(server, UA_NODECLASS_VARIABLE, @requestedNewNodeId,
    @parentNodeId, @referenceTypeId, browseName,
    @typeDefinition, PUA_NodeAttributes(@attr),
    @UA_TYPES[UA_TYPES_VARIABLEATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addObjectTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr:UA_ObjectTypeAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result :=__UA_Server_addNode(server, UA_NODECLASS_OBJECTTYPE, @requestedNewNodeId,
    @parentNodeId, @referenceTypeId, browseName,
    @UA_NODEID_NULL, @attr,
    @UA_TYPES[UA_TYPES_OBJECTTYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;


function UA_Server_addObjectNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: UA_NodeId;
  const attr:UA_ObjectAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result :=__UA_Server_addNode(server, UA_NODECLASS_OBJECT, @requestedNewNodeId,
    @parentNodeId, @referenceTypeId, browseName,
    @typeDefinition, @attr,
    @UA_TYPES[UA_TYPES_OBJECTATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addVariableTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const typeDefinition: UA_NodeId;
  const attr:UA_VariableTypeAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result :=__UA_Server_addNode(server, UA_NODECLASS_VARIABLETYPE,
    @requestedNewNodeId, @parentNodeId, @referenceTypeId,
    browseName, @typeDefinition,
    @attr,
    @UA_TYPES[UA_TYPES_VARIABLETYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addViewNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr:UA_ViewAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result :=__UA_Server_addNode(server, UA_NODECLASS_VIEW, @requestedNewNodeId,
    @parentNodeId, @referenceTypeId, browseName,
    @UA_NODEID_NULL, @attr,
    @UA_TYPES[UA_TYPES_VIEWATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addReferenceTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr: UA_ReferenceTypeAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result :=__UA_Server_addNode(server, UA_NODECLASS_REFERENCETYPE,
    @requestedNewNodeId, @parentNodeId, @referenceTypeId,
    browseName, @UA_NODEID_NULL,
    @attr,
    @UA_TYPES[UA_TYPES_REFERENCETYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addDataTypeNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr:UA_DataTypeAttributes;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result :=__UA_Server_addNode(server, UA_NODECLASS_DATATYPE, @requestedNewNodeId,
    @parentNodeId, @referenceTypeId, browseName,
    @UA_NODEID_NULL, @attr,
    @UA_TYPES[UA_TYPES_DATATYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_writeValue(server: PUA_Server;
  const nodeId: UA_NodeId;
  const value: UA_Variant): UA_StatusCode;
begin
  Result := __UA_Server_write(server, @nodeId, UA_ATTRIBUTEID_VALUE,
    @UA_TYPES[UA_TYPES_VARIANT], @value);
end;

function UA_Server_addMethodNode(server: PUA_Server;
  const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr: UA_MethodAttributes;
  method: UA_MethodCallback;
  inputArgumentsSize: size_t;
  const inputArguments: PUA_Argument;
  outputArgumentsSize: size_t;
  const outputArguments: PUA_Argument;
  nodeContext: Pointer;
  outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := UA_Server_addMethodNodeEx(server, requestedNewNodeId, parentNodeId,
    referenceTypeId, browseName, attr, method,
    inputArgumentsSize, inputArguments, UA_NODEID_NULL, nil,
    outputArgumentsSize, outputArguments, UA_NODEID_NULL, nil,
    nodeContext, outNewNodeId);
end;
{$ENDIF}


end.
