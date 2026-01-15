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

{$DEFINE LOAD_DYNAMICALLY}

{$IFDEF FPC}
  {$IFDEF LOAD_DYNAMICALLY}
    {$MODE DELPHI}
  {$ENDIF}
  {$PACKENUM 4} // GCC on x86 enums have size of 4 bytes
  {$PACKRECORDS C}
{$ELSE}
  {$MINENUMSIZE 4}
  {$ALIGN 4}
{$ENDIF}
{$H+}
{$POINTERMATH ON}

{$DEFINE ENABLE_SERVER}

// Use open62541 v1.4
{ $DEFINE UA_VER1_4}
// Use open62541 v1.3
{ $DEFINE UA_VER1_3}
{$IFNDEF UA_VER1_4}
  {$IFNDEF UA_VER1_3}
    {$DEFINE UA_VER1_2}
  {$ENDIF}
{$ENDIF}


interface


// --- config.h ---
{$DEFINE UA_ENABLE_METHODCALLS}
{$DEFINE UA_ENABLE_SUBSCRIPTIONS}
{$DEFINE UA_ENABLE_STATUSCODE_DESCRIPTIONS}
{$DEFINE UA_ENABLE_TYPEDESCRIPTION}

// disabled in pre-compiled "libopen62541" library (required for SIGN and SIGN&ENCRYPT)
{ $DEFINE UA_ENABLE_ENCRYPTION} 


const

{$IFDEF UNIX}
  libopen62541 = 'libopen62541.so';
{$ELSE}
  // GCC libgcc_s_sjlj-1.dll and libwinpthread-1.dll are also required
  //  they can be downloaded from packages at http://win-builds.org/1.5.0/packages/windows_32/
  libopen62541 = 'libopen62541.dll'; 
{$ENDIF}

// Library version
{$IFDEF UA_VER1_4}
  UA_VER = 1.4;
{$ELSE}
  {$IFDEF UA_VER1_3}
  UA_VER = 1.3;
  {$ELSE}
  UA_VER = 1.2;
  {$ENDIF}
{$ENDIF}


type

  {$IFNDEF FPC}
  // Delphi XE compatibility
  // FixedUInt 32-bit
  DWord = LongWord; 
  {$ENDIF}
  // DWord on 32-bit platforms, QWord on 64-bit platforms
  size_t = NativeUInt;

  UA_Client = record end;
  PUA_Client = ^UA_Client;


  // --- types.h ---

  UA_Boolean = bytebool;
  PUA_Boolean = ^UA_Boolean;
  
  UA_Byte = Byte;       
  PUA_Byte = ^UA_Byte;
  
  UA_Int16 = Smallint;  
  PUA_Int16 = ^UA_Int16;
  
  UA_UInt16 = Word;     
  PUA_UInt16 = ^UA_UInt16;
  
  UA_Int32 = Integer;   
  PUA_Int32 = ^UA_Int32;
  
  UA_UInt32 = DWord;    
  PUA_UInt32 = ^UA_UInt32;
  
  UA_Int64 = Int64;     
  PUA_Int64 = ^UA_Int64;
  
  UA_UInt64 = UInt64;   
  PUA_UInt64 = ^UA_UInt64;
  
  UA_Float = single;    
  PUA_Float = ^UA_Float;
  
  UA_Double = double;   
  PUA_Double = ^UA_Double;


  // StatusCode (.. _statuscode:)
  //
  // A numeric identifier for a error or condition that is associated with a value
  // or an operation. See the section :ref:`statuscodes` for the meaning of a specific code.
  UA_StatusCode = DWord; // uint32_t
  PUA_StatusCode = ^UA_StatusCode;

  // String - A sequence of Unicode characters. Strings are just an array of UA_Byte.
  UA_String = record
    length: size_t; // The length of the string
    data: PUA_Byte; // The content (not null-terminated)
  end;
  PUA_String = ^UA_String;

  // DateTime (.. _datetime:)
  //
  // An instance in time. A DateTime value is encoded as a 64-bit signed integer
  // which represents the number of 100 nanosecond intervals since January 1, 1601 (UTC).
  //
  // The methods providing an interface to the system clock are architecture-
  // specific. Usually, they provide a UTC clock that includes leap seconds. The
  // OPC UA standard allows the use of International Atomic Time (TAI) for the
  // DateTime instead. But this is still unusual and not implemented for most
  // SDKs. Currently (2019), UTC and TAI are 37 seconds apart due to leap seconds.
  UA_DateTime = Int64;
  PUA_DateTime = ^UA_DateTime;

  UA_DateTimeStruct = record
    nanoSec: UA_UInt16;
    microSec: UA_UInt16;
    milliSec: UA_UInt16;
    sec: UA_UInt16;
    min: UA_UInt16;
    hour: UA_UInt16;
    day: UA_UInt16;
    month: UA_UInt16;
    year: UA_UInt16;
  end;

  // ByteString
  // A sequence of octets.
  UA_ByteString = UA_String;
  PUA_ByteString = ^UA_ByteString;

  // GUID
  // A 16 byte value that can be used as a globally unique identifier.
  UA_Guid = record
    data1: UA_UInt32;
    data2: UA_UInt16;
    data3: UA_UInt16 ;
    data4: array[0..7] of UA_Byte;
  end;

  UA_LogLevel = (
    UA_LOGLEVEL_TRACE,
    UA_LOGLEVEL_DEBUG,
    UA_LOGLEVEL_INFO,
    UA_LOGLEVEL_WARNING,
    UA_LOGLEVEL_ERROR,
    UA_LOGLEVEL_FATAL
  );

  UA_LogCategory = (
    UA_LOGCATEGORY_NETWORK,
    UA_LOGCATEGORY_SECURECHANNEL,
    UA_LOGCATEGORY_SESSION,
    UA_LOGCATEGORY_SERVER,
    UA_LOGCATEGORY_CLIENT,
    UA_LOGCATEGORY_USERLAND,
    UA_LOGCATEGORY_SECURITYPOLICY
  );

  UA_Logger = record
    // Log a message. The message string and following varargs are formatted
    // according to the rules of the printf command. Use the convenience macros
    // below that take the minimum log-level defined in ua_config.h into account.
    log: procedure(logContext: Pointer; level: UA_LogLevel; category: UA_LogCategory; msg: PAnsiChar; args: {va_list}array of const); cdecl;
    context: Pointer; // Logger state
    clear: procedure(context: Pointer); cdecl; // Clean up the logger plugin
  end;
  PUA_Logger  = ^UA_Logger;

  // NodeId (.. _nodeid:)
  //
  // An identifier for a node in the address space of an OPC UA Server.
  UA_NodeIdType = (
    // In the binary encoding, this can also
    // become 1 or 2 (two-byte and four-byte
    // encoding of small numeric nodeids)
    UA_NODEIDTYPE_NUMERIC    = 0,
    UA_NODEIDTYPE_STRING     = 3,
    UA_NODEIDTYPE_GUID       = 4,
    UA_NODEIDTYPE_BYTESTRING = 5
  );

  UA_NodeId = record
    namespaceIndex: UA_UInt16;
    identifierType: UA_NodeIdType;
    identifier: record
        case longint of
          0: ( numeric: UA_UInt32 );
          1: ( _string: UA_String );
          2: ( guid: UA_Guid );
          3: ( byteString: UA_ByteString );
        end;
  end;
  PUA_NodeId = ^UA_NodeId;

  // ExpandedNodeId
  //
  // A NodeId that allows the namespace URI to be specified instead of an index.
  UA_ExpandedNodeId = record
    nodeId: UA_NodeId;
    namespaceUri: UA_String;
    serverIndex: UA_UInt32;
  end;
  PUA_ExpandedNodeId = ^UA_ExpandedNodeId;

  // QualifiedName (.. _qualifiedname:)
  //
  // A name qualified by a namespace. *)
  UA_QualifiedName = record
    namespaceIndex: UA_UInt16;
    name: UA_String;
  end;
  PUA_QualifiedName = ^UA_QualifiedName;

  // LocalizedText
  //
  // Human readable text with an optional locale identifier.
  UA_LocalizedText = record
    Locale: UA_String;
    Text: UA_String;
  end;
  PUA_LocalizedText = ^UA_LocalizedText;

  // NumericRange
  //
  // NumericRanges are used to indicate subsets of a (multidimensional) array.
  // They no official data type in the OPC UA standard and are transmitted only
  // with a string encoding, such as "1:2,0:3,5". The colon separates min/max
  // index and the comma separates dimensions. A single value indicates a range
  // with a single element (min==max). *)
  UA_NumericRangeDimension = record
    min: UA_UInt32;
    max: UA_UInt32;
  end;

  UA_NumericRange = record
    dimensionsSize: size_t;
    dimensions: ^UA_NumericRangeDimension;
  end;
  PUA_NumericRange = ^UA_NumericRange;


  PUA_DataType = ^UA_DataType;

  end;
  PUA_Variant = ^UA_Variant;

  // ExtensionObject (.. _extensionobject:)
  //
  // ExtensionObjects may contain scalars of any data type. Even those that are
  // unknown to the receiver. See the section on :ref:`generic-types` on how types
  // are described. If the received data type is unknown, the encoded string and
  // target NodeId is stored instead of the decoded value. *)
  UA_ExtensionObjectEncoding = (
    UA_EXTENSIONOBJECT_ENCODED_NOBODY     = 0,
    UA_EXTENSIONOBJECT_ENCODED_BYTESTRING = 1,
    UA_EXTENSIONOBJECT_ENCODED_XML        = 2,
    UA_EXTENSIONOBJECT_DECODED            = 3,
    UA_EXTENSIONOBJECT_DECODED_NODELETE   = 4 // Don't delete the content together with the ExtensionObject
  );

  UA_ExtensionObject = record
    encoding: UA_ExtensionObjectEncoding;
    content:  record
                case LongInt of

  PUA_ExtensionObject = ^UA_ExtensionObject;

  // DataValue
  //
  // A data value with an associated status code and timestamps.
  UA_DataValue = record

  end;
  PUA_DataValue = ^UA_DataValue;

  // DiagnosticInfo - A structure that contains detailed error and diagnostic information
  // associated with a StatusCode.
  PUA_DiagnosticInfo = ^UA_DiagnosticInfo;
  UA_DiagnosticInfo = record


  {$IFDEF UA_VER1_3}
  UA_DataTypeMember = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}

                                    namespace zero. In this implementation,
                                    types from custom namespace may contain
                                    members from the same namespace or
                                    namespace zero only.*)

{$ENDIF}


  // --- common.h ---

type

  //
  // Standard-Defined Constants
  //
  // This section contains numerical and string constants that are defined in the
  // OPC UA standard.


  // Attribute Id (.. _attribute-id:)
  //
  // Every node in an OPC UA information model contains attributes depending on
  // the node type. Possible attributes are as follows:

  UA_AttributeId = (
    UA_ATTRIBUTEID_NODEID                  = 1,
    UA_ATTRIBUTEID_NODECLASS               = 2,
    UA_ATTRIBUTEID_BROWSENAME              = 3,
    UA_ATTRIBUTEID_DISPLAYNAME             = 4,
    UA_ATTRIBUTEID_DESCRIPTION             = 5,
    UA_ATTRIBUTEID_WRITEMASK               = 6,
    UA_ATTRIBUTEID_USERWRITEMASK           = 7,
    UA_ATTRIBUTEID_ISABSTRACT              = 8,
    UA_ATTRIBUTEID_SYMMETRIC               = 9,
    UA_ATTRIBUTEID_INVERSENAME             = 10,
    UA_ATTRIBUTEID_CONTAINSNOLOOPS         = 11,
    UA_ATTRIBUTEID_EVENTNOTIFIER           = 12,
    UA_ATTRIBUTEID_VALUE                   = 13,
    UA_ATTRIBUTEID_DATATYPE                = 14,
    UA_ATTRIBUTEID_VALUERANK               = 15,
    UA_ATTRIBUTEID_ARRAYDIMENSIONS         = 16,
    UA_ATTRIBUTEID_ACCESSLEVEL             = 17,
    UA_ATTRIBUTEID_USERACCESSLEVEL         = 18,
    UA_ATTRIBUTEID_MINIMUMSAMPLINGINTERVAL = 19,
    UA_ATTRIBUTEID_HISTORIZING             = 20,
    UA_ATTRIBUTEID_EXECUTABLE              = 21,
    UA_ATTRIBUTEID_USEREXECUTABLE          = 22,
    UA_ATTRIBUTEID_DATATYPEDEFINITION      = 23
  );


  );


  // Connection State
  UA_SecureChannelState = (
    UA_SECURECHANNELSTATE_CLOSED,
    UA_SECURECHANNELSTATE_HEL_SENT,
    UA_SECURECHANNELSTATE_HEL_RECEIVED,
    UA_SECURECHANNELSTATE_ACK_SENT,
    UA_SECURECHANNELSTATE_ACK_RECEIVED,
    UA_SECURECHANNELSTATE_OPN_SENT,
    UA_SECURECHANNELSTATE_OPEN,
    UA_SECURECHANNELSTATE_CLOSING
  );
  PUA_SecureChannelState = ^UA_SecureChannelState;

  UA_SessionState = (
    UA_SESSIONSTATE_CLOSED,
    UA_SESSIONSTATE_CREATE_REQUESTED,
    UA_SESSIONSTATE_CREATED,
    UA_SESSIONSTATE_ACTIVATE_REQUESTED,
    UA_SESSIONSTATE_ACTIVATED,
    UA_SESSIONSTATE_CLOSING
  );
  PUA_SessionState = ^UA_SessionState;

  // Statistic counters
  //
  // The stack manage statistic counter for the following layers:
  // - Network
  // - Secure channel
  // - Session
  //
  // The session layer counters are matching the counters of the
  // ServerDiagnosticsSummaryDataType that are defined in the OPC UA Part 5
  // specification. Counter of the other layers are not specified by OPC UA but
  // are harmonized with the session layer counters if possible.
  UA_NetworkStatistics = record

  end;
  PUA_NetworkStatistics = ^UA_NetworkStatistics;

  UA_SecureChannelStatistics = record

  end;
  PUA_SecureChannelStatistics = ^UA_SecureChannelStatistics;

  UA_SessionStatistics = record

  end;
  PUA_SessionStatistics = ^UA_SessionStatistics;


  // --- network.h ---

  // Networking Plugin API (.. _networking:)

  // Connection
  //
  // Client-server connections are represented by a `UA_Connection`. The
  // connection is stateful and stores partially received messages, and so on. In
  // addition, the connection contains function pointers to the underlying
  // networking implementation. An example for this is the `send` function. So the
  // connection encapsulates all the required networking functionality. This lets
  // users on embedded (or otherwise exotic) systems implement their own
  // networking plugins with a clear interface to the main open62541 library.

  UA_ConnectionConfig = record
    protocolVersion: UA_UInt32;
    recvBufferSize: UA_UInt32;
    sendBufferSize: UA_UInt32;

  end;
  {$ENDIF}


  // --- securitypolicy.h ---

  PUA_SecurityPolicy = ^UA_SecurityPolicy;
  UA_SecurityPolicy = record
      // Additional data
      policyContext: Pointer;

      // The policy uri that identifies the implemented algorithms
      policyUri: UA_ByteString;

      // The local certificate is specific for each SecurityPolicy since it
      // depends on the used key length.
      localCertificate: UA_ByteString;

      // Function pointers grouped into modules
{ TODO:
      UA_SecurityPolicyAsymmetricModule asymmetricModule;
      UA_SecurityPolicySymmetricModule symmetricModule;
      UA_SecurityPolicySignatureAlgorithm certificateSigningAlgorithm;
      UA_SecurityPolicyChannelModule channelModule;
      UA_CertificateVerification *certificateVerification;
}
      logger: PUA_Logger;

      // Updates the ApplicationInstanceCertificate and the corresponding private
      // key at runtime.
      updateCertificateAndPrivateKey: function(
        policy: PUA_SecurityPolicy;
        const newCertificate: UA_ByteString;
        const newPrivateKey: UA_ByteString): UA_StatusCode;

      // Deletes the dynamic content of the policy
      deleteMembers: procedure(policy: PUA_SecurityPolicy);
  end;


  // --- plugin/pki.h ---

  PUA_CertificateVerification  = ^UA_CertificateVerification;
  UA_CertificateVerification = record

  end;


  // --- client_config.h ---

  UA_ClientConfig = record

    {$ENDIF}
  end;
  PUA_ClientConfig = ^UA_ClientConfig;

  {$IFDEF UA_ENABLE_SUBSCRIPTIONS}

  {$ENDIF}

  {$IFDEF ENABLE_SERVER}
  // --- server.h ---

  UA_Server = record end;

  end;
  PUA_ServerConfig = ^UA_ServerConfig;
  {$ENDIF ENABLE_SERVER}

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
  {$I statuscodes.inc}


  // --- nodeids.h ---
  {$I nodeids.inc}



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


  // --- types.h ---

  UA_EMPTY_ARRAY_SENTINEL = Pointer($01);

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
  UA_ClientConfig_setDefaultEncryption: function(config: PUA_ClientConfig; localCertificate, privateKey: UA_ByteString; trustList: PUA_ByteString; trustListSize: Size_T; revocationList: PUA_ByteString; revocationListSize: Size_T): UA_StatusCode; cdecl;
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
  UA_Variant_setArray: procedure(v: PUA_Variant; arrayData: Pointer; arraySize: Size_T; _type: PUA_DataType); cdecl;
  UA_Variant_setArrayCopy: function(v: PUA_Variant; arrayData: Pointer; arraySize: Size_T; _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_DateTime_toStruct: function(t: UA_DateTime): UA_DateTimeStruct; cdecl;
  UA_DateTime_fromStruct: function(ts: UA_DateTimeStruct): UA_DateTime; cdecl;
  UA_findDataType: function(typeId: PUA_NodeId): PUA_DataType; cdecl;

  UA_new: function(const _type: PUA_DataType): Pointer; cdecl;
  UA_copy: function(src,dst: Pointer; const _type: PUA_DataType): UA_StatusCode; cdecl;
  UA_clear: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_delete: procedure(p: Pointer; const _type: PUA_DataType); cdecl;
  UA_Array_delete: procedure(p: Pointer; size: Size_T; const _type: PUA_DataType); cdecl;

  __UA_Client_readAttribute: function(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl;
  UA_Client_readArrayDimensionsAttribute: function(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: Size_T; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl;

  __UA_Client_writeAttribute: function(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl;
  UA_Client_writeArrayDimensionsAttribute: function(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: Size_T; newArrayDimensions: PUA_UInt32): UA_StatusCode;
  {$IFDEF UA_ENABLE_METHODCALLS}
  UA_Client_call: function(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: Size_T; input: PUA_Variant; out outputSize: Size_T; out output: PUA_Variant): UA_StatusCode; cdecl;
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


// --- client.h --- }

// Client(.. _client:)
// ======
//
// The client implementation allows remote access to all OPC UA services. For
// convenience, some functionality has been wrapped in :ref:`high-level
// abstractions <client-highlevel>`.
//
// **However**: At this time, the client does not yet contain its own thread or
// event-driven main-loop. So the client will not perform any actions
// automatically in the background. This is especially relevant for
// subscriptions. The user will have to periodically call
// `UA_Client_Subscriptions_manuallySendPublishRequest`. See also :ref:`here
// <client-subscriptions>`.
//
// Client Lifecycle (.. include:: client_config.rst)

// Create a new client
function UA_Client_new(): PUA_Client; cdecl; external libopen62541;

// Creates a new client. Moves the config into the client with a shallow copy.
// The config content is cleared together with the client.
function UA_Client_newWithConfig(const config: PUA_ClientConfig): PUA_Client; cdecl; external libopen62541;

// Returns the current state. All arguments except ``client`` can be NULL
procedure UA_Client_getState(client: PUA_Client; channelState:
  PUA_SecureChannelState; sessionState: PUA_SessionState;
  connectStatus: PUA_StatusCode); cdecl; external libopen62541;

// Get the client configuration
function UA_Client_getConfig(client: PUA_Client): PUA_ClientConfig; cdecl; external libopen62541;

function UA_ClientConfig_setDefault(config: PUA_ClientConfig): UA_StatusCode; cdecl; external libopen62541;
{$IFDEF UA_ENABLE_ENCRYPTION}

{$ENDIF}

// Delete a client
procedure UA_Client_delete(client: PUA_Client); cdecl; external libopen62541;

// Connect to the server
//
// @param client to use
// @param endpointURL to connect (for example "opc.tcp://localhost:4840")
// @return Indicates whether the operation succeeded or returns an error code
function UA_Client_connect(client: PUA_Client; const endpointUrl: AnsiString): UA_StatusCode; cdecl; external libopen62541;
// Connect to the selected server with the given username and password
//function UA_Client_connect_username(client: PUA_Client; endpointUrl: PAnsiChar;
  username, password: PAnsiChar): UA_StatusCode; cdecl; external libopen62541; deprecated;
// Disconnect and close a connection to the selected server
function UA_Client_disconnect(client: PUA_Client): UA_StatusCode; cdecl; external libopen62541;

// Services (.. _client-services:)
//
// The raw OPC UA services are exposed to the client. But most of them time, it
// is better to use the convenience functions from ``ua_client_highlevel.h``
// that wrap the raw services.

// Don't use this function. Use the type versions below instead
procedure __UA_Client_Service(client: PUA_Client; const request: Pointer;
  const requestType: PUA_DataType; response: Pointer;
  const responseType: PUA_DataType); cdecl; external libopen62541;

// Asynchronous Services
//
// All OPC UA services are asynchronous in nature. So several service calls can
// be made without waiting for a response first. Responess may come in a
// different ordering.

// Listen on the network and process arriving asynchronous responses in the
// background. Internal housekeeping, renewal of SecureChannels and subscription
// management is done as well
function UA_Client_run_iterate(client: PUA_Client; timeout: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;


// --- types.h ---

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



// --- client_highlevel.h ---

function __UA_Client_readAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_readArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: Size_T; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;

function __UA_Client_writeAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_writeArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: Size_T; newArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;
{$IFDEF UA_ENABLE_METHODCALLS}
function UA_Client_call(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: Size_T; input: PUA_Variant; out outputSize: Size_T; out output: PUA_Variant): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}


// --- plugin/securitypolicy.h ---

function UA_SecurityPolicy_None(policy: PUA_SecurityPolicy; certificateVerification: PUA_CertificateVerification; const localCertificate: UA_ByteString; const logger: PUA_Logger): UA_StatusCode; cdecl; external libopen62541;

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
// Delete a single subscription
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
// Executes a single iteration of the server's main loop. *)
function UA_Server_run_iterate(server: PUA_Server;
  waitInternal: UA_Boolean): UA_UInt16; cdecl; external libopen62541;
// The epilogue part of UA_Server_run (no need to use if you call UA_Server_run)
function UA_Server_run_shutdown(server: PUA_Server): UA_StatusCode; cdecl; external libopen62541;

function __UA_Server_addNode(server: PUA_Server; const nodeClass: UA_NodeClass;

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

function UA_Variant_getFloat(var v: UA_Variant): single;
function UA_Variant_getDouble(var v: UA_Variant): double;
function UA_Variant_getByte(var v: UA_Variant): Byte;
function UA_Variant_getSmallint(var v: UA_Variant): Smallint;
function UA_Variant_getInteger(var v: UA_Variant): Integer;
function UA_Variant_getInt64(var v: UA_Variant): Int64;
function UA_Variant_getString(var v: UA_Variant): AnsiString; overload;
function UA_Variant_getString(var v: UA_Variant; arrayIndex: DWord): AnsiString; overload;
procedure UA_Variant_setBoolean(out v: UA_Variant; b: bytebool);
procedure UA_Variant_setFloat(out v: UA_Variant; f: single);
procedure UA_Variant_setDouble(out v: UA_Variant; d: double);
procedure UA_Variant_setByte(out v: UA_Variant; i: Byte);
procedure UA_Variant_setSmallint(out v: UA_Variant; i: Smallint);
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
  const nodeId: UA_NodeId; out outValue: Smallint): UA_StatusCode; overload;
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
  const nodeId: UA_NodeId; const newValue: Smallint): UA_StatusCode; overload;
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
function UA_MonitoredItemCreateRequest_default(NodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
{$ENDIF}

{$IFDEF ENABLE_SERVER}
// --- server_config_default.h ---

function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;

{$ENDIF}


implementation


{$IFDEF LOAD_DYNAMICALLY}
uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows
{$ELSE}
  DynLibs
{$ENDIF}
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

end;

function _UA_STRING_ALLOC(const chars: AnsiString): UA_String; inline;
var
  UABStr: UA_BYTESTRING;
begin

end;

function _UA_BYTESTRING(var chars: AnsiString): UA_ByteString;
begin
  Result := UA_ByteString(_UA_STRING(chars));
end;

function _UA_BYTESTRING_ALLOC(const chars: AnsiString): UA_ByteString; inline;
begin
  Result := UA_ByteString(_UA_STRING_ALLOC(chars));
end;

function _UA_QUALIFIEDNAME(nsIndex: UA_UInt16; var chars: AnsiString): UA_QualifiedName;
begin
  Result.NamespaceIndex := nsIndex;
  Result.Name := _UA_STRING(chars);
end;

function _UA_QUALIFIEDNAME_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_QualifiedName;
begin
  Result.NamespaceIndex := nsIndex;
  Result.Name := _UA_STRING_ALLOC(chars);
end;

function _UA_LOCALIZEDTEXT(var locale, text: AnsiString): UA_LocalizedText;
begin
  Result.Locale := _UA_STRING(locale);
  Result.Text := _UA_STRING(text);
end;

function _UA_LOCALIZEDTEXT_ALLOC(const locale, text: AnsiString): UA_LocalizedText;
begin
  Result.Locale := _UA_STRING_ALLOC(locale);
  Result.Text := _UA_STRING_ALLOC(text)
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
  SetString(Result, PAnsiChar(s.Data), s.Length);
end;

function UA_LocalizedTextToStr(const t: UA_LocalizedText): AnsiString;
begin
  SetString(Result, PAnsiChar(t.Text.Data), t.Text.Length);
end;

function UA_NodeIdToStr(const id: UA_NodeId): AnsiString;
var 
  Output: UA_String;
begin

end;

function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
var
  PDataType: PUA_DataType;
begin

procedure UA_Variant_init(out p: UA_Variant);
begin
  FillChar(p, SizeOf(UA_Variant), #0);
end;


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


procedure UA_BrowseResponse_init(out p: UA_BrowseResponse);
begin
  FillChar(p, SizeOf(p), #0);
end;


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


procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
begin
  FillChar(p, SizeOf(p), #0);
end;


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
  Result := v^._Type = nil;
end;

function UA_Variant_isScalar(const v: PUA_Variant): Boolean;
begin
  Result := (v^.ArrayLength = 0) and (PByte(v^.Data) > PByte(UA_EMPTY_ARRAY_SENTINEL));
end;

function UA_Variant_hasScalarType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
begin
  Result := UA_Variant_isScalar(v) and (_type = v^._Type);
end;

function UA_Variant_hasArrayType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
begin
  Result := (not UA_Variant_isScalar(v)) and (_type = v^._Type);
end;

function UA_Variant_getFloat(var v: UA_Variant): single;
begin
  Result := PUA_Float(v.Data)^;
end;

function UA_Variant_getDouble(var v: UA_Variant): double;
begin
  Result := PUA_Double(v.Data)^;
end;

function UA_Variant_getByte(var v: UA_Variant): Byte;
begin
  Result := PUA_Byte(v.Data)^;
end;

function UA_Variant_getSmallint(var v: UA_Variant): Smallint;
begin
  Result := PUA_Int16(v.Data)^;
end;

function UA_Variant_getInteger(var v: UA_Variant): Integer;
begin
  Result := PUA_Int32(v.Data)^;
end;

function UA_Variant_getInt64(var v: UA_Variant): Int64;
begin
  Result := PUA_Int64(v.Data)^;
end;

function UA_Variant_getString(var v: UA_Variant): AnsiString;
begin
  SetString(Result, PAnsiChar(PUA_String(v.Data)^.Data), PUA_String(v.Data)^.Length);
end;

function UA_Variant_getString(var v: UA_Variant; arrayIndex: DWord): AnsiString;
begin
  if arrayIndex < v.ArrayLength then
    SetString(Result, PAnsiChar(PUA_String(v.Data)[arrayIndex].Data), PUA_String(v.Data)[arrayIndex].Length)
  else
    Result := '';
end;

procedure UA_Variant_setBoolean(out v: UA_Variant; b: bytebool);
begin
  UA_Variant_setScalarCopy(@v, @b, @UA_TYPES[UA_TYPES_BOOLEAN]);
end;

procedure UA_Variant_setFloat(out v: UA_Variant; f: single);
begin
  UA_Variant_setScalarCopy(@v, @f, @UA_TYPES[UA_TYPES_FLOAT]);
end;

procedure UA_Variant_setDouble(out v: UA_Variant; d: double);
begin
  UA_Variant_setScalarCopy(@v, @d, @UA_TYPES[UA_TYPES_DOUBLE]);
end;

procedure UA_Variant_setByte(out v: UA_Variant; i: Byte);
begin
  UA_Variant_setScalarCopy(@v, @i, @UA_TYPES[UA_TYPES_BYTE]);
end;

procedure UA_Variant_setSmallint(out v: UA_Variant; i: Smallint);
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

function UA_NODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_NodeId;
begin
  Result.NamespaceIndex := nsIndex;
  Result.IdentifierType := UA_NODEIDTYPE_NUMERIC;
  Result.Identifier.Numeric := identifier;
end;

function UA_NODEID_STRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
begin
  Result.NamespaceIndex := nsIndex;
  Result.IdentifierType := UA_NODEIDTYPE_STRING;
  Result.Identifier._String := _UA_STRING(chars);
end;

function UA_NODEID_STRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
begin
  Result.NamespaceIndex := nsIndex;
  Result.IdentifierType := UA_NODEIDTYPE_STRING;
  Result.Identifier._String := _UA_STRING_ALLOC(chars);
end;

function UA_NODEID_GUID(nsIndex: UA_UInt16; guid: UA_Guid): UA_NodeId;
begin
  Result.NamespaceIndex := nsIndex;
  Result.IdentifierType := UA_NODEIDTYPE_GUID;
  Result.Identifier.GUID := guid;
end;

function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
begin
  Result.NamespaceIndex := nsIndex;
  Result.IdentifierType := UA_NODEIDTYPE_BYTESTRING;
  Result.Identifier.ByteString := _UA_BYTESTRING(chars);
end;

function UA_NODEID_BYTESTRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
begin
  Result.NamespaceIndex := nsIndex;
  Result.IdentifierType := UA_NODEIDTYPE_BYTESTRING;
  Result.Identifier.ByteString := _UA_BYTESTRING_ALLOC(chars);
end;

function UA_EXPANDEDNODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_Uint32): UA_ExpandedNodeId;
begin

end;

procedure UA_init(p: Pointer; const _type: PUA_DataType);
begin
  FillChar(p^, _type^.MemSize, #0);
end;

function UA_Client_connect_username(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode;
begin
  Result := UA_Client_connectUsername(client, endpointUrl, username, password);
end;


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
  const nodeId: UA_NodeId; out outValue: UA_Variant): UA_StatusCode; overload;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE,
    @outValue, @UA_TYPES[UA_TYPES_VARIANT]);
end;


// taken from ua_client_highlevel.c: __UA_Client_readAttribute()
// (use to read subrange of array variable)
function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const indexRange: AnsiString;
  out outValue: UA_Variant): UA_StatusCode; overload;
var

    else
      Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
  end;

  if Result = UA_STATUSCODE_GOOD then
  begin
    (* Set the StatusCode *)

    end
    else
      Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
  end;

  UA_clear(@Response, @UA_TYPES[UA_TYPES_READRESPONSE]);
  UA_String_clear(Item.indexRange);
end;

    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(Value);
end;

function UA_Client_readValueAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValue: Smallint): UA_StatusCode;
var
  Value: UA_Variant;
begin

    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(Value);
end;


    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(Value);
end;


    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(Value);
end;

function UA_Client_readDataTypeAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outDataType: UA_NodeId): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DATATYPE,
    @outDataType, @UA_TYPES[UA_TYPES_NODEID]);
end;

function UA_Client_readValueRankAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outValueRank: UA_Int32): UA_StatusCode;
begin
  // -2:Any; -1:Scalar; 0:OneOrMoreDimensions; 1:OneDimension
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUERANK,
    @outValueRank, @UA_TYPES[UA_TYPES_INT32]);
end;

function UA_Client_readBrowseNameAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outBrowseName: UA_QualifiedName): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_BROWSENAME,
    @outBrowseName, @UA_TYPES[UA_TYPES_QUALIFIEDNAME]);
end;

function UA_Client_readDisplayNameAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outDisplayName: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DISPLAYNAME,
    @outDisplayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;

function UA_Client_readDescriptionAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; out outDescription: UA_LocalizedText): UA_StatusCode;
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


begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_BYTE]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;


begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_INT16]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;


begin
  UA_Variant_setScalar(@UAVar, @newValue, @UA_TYPES[UA_TYPES_INT32]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
end;


begin
  UAStr := _UA_STRING_ALLOC(newValue);
  UA_Variant_setScalar(@UAVar, @UAStr, @UA_TYPES[UA_TYPES_STRING]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
  UA_String_clear(UAStr);
end;


begin
  SetLength(UAStr, Length(newValues));
  for i := Low(newValues) to High(newValues) do
    UAStr[i] := _UA_STRING_ALLOC(newValues[i]);
  UA_Variant_setArray(@UAVar, @UAStr[0], Length(UAStr), @UA_TYPES[UA_TYPES_STRING]);
  Result := UA_Client_writeValueAttribute(client, nodeId, UAVar);
  for i := Low(newValues) to High(newValues) do
    UA_String_clear(UAStr[i]);
end;


function UA_Client_writeDescriptionAttribute(client: PUA_Client;
  const nodeId: UA_NodeId;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newDescription: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_DESCRIPTION,
    @newDescription, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;

function UA_Client_writeDataTypeAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; newDataType: PUA_NodeId): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_DATATYPE,
    newDataType, @UA_TYPES[UA_TYPES_NODEID]);
end;

function UA_Client_writeValueRankAttribute(client: PUA_Client;
  const nodeId: UA_NodeId; const newValueRank: UA_Int32): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUERANK,
    @newValueRank, @UA_TYPES[UA_TYPES_INT32]);
end;

{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
begin
  UA_CreateSubscriptionRequest_init(Result);


end;

function UA_MonitoredItemCreateRequest_default(NodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
begin
  UA_MonitoredItemCreateRequest_init(Result);


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
 * The config will set the tcp network layer to the given port and adds a single
 * endpoint with the security policy ``SecurityPolicy#None`` to the server. A
 * server certificate may be supplied but is optional. *)
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig;
  portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;
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
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := __UA_Server_addNode(server, UA_NODECLASS_VARIABLE, @requestedNewNodeId,

    @requestedNewNodeId, @parentNodeId, @referenceTypeId,
    browseName, @typeDefinition,
    @attr,
    @UA_TYPES[UA_TYPES_VARIABLETYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;


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

    @requestedNewNodeId, @parentNodeId, @referenceTypeId,
    browseName, @UA_NODEID_NULL,
    @attr,
    @UA_TYPES[UA_TYPES_REFERENCETYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;


    referenceTypeId, browseName, attr, method,
    inputArgumentsSize, inputArguments, UA_NODEID_NULL, nil,
    outputArgumentsSize, outputArguments, UA_NODEID_NULL, nil,
    nodeContext, outNewNodeId);
end;
{$ENDIF}


end.

