(* Generated from Opc.Ua.Types.bsd with script ./generate_datatypes_fpc.py
* on host seis by user luca at 2021-11-04 04:52:46 *)

(**
 * Every type is assigned an index in an array containing the type descriptions.
 * These descriptions are used during type handling (copying, deletion,
 * binary encoding, ...). *)
const
  UA_TYPES_COUNT = 190;

(**
 * Boolean
 * ^^^^^^^
 *)
  UA_TYPES_BOOLEAN = 0;

(**
 * SByte
 * ^^^^^
 *)
  UA_TYPES_SBYTE = 1;

(**
 * Byte
 * ^^^^
 *)
  UA_TYPES_BYTE = 2;

(**
 * Int16
 * ^^^^^
 *)
  UA_TYPES_INT16 = 3;

(**
 * UInt16
 * ^^^^^^
 *)
  UA_TYPES_UINT16 = 4;

(**
 * Int32
 * ^^^^^
 *)
  UA_TYPES_INT32 = 5;

(**
 * UInt32
 * ^^^^^^
 *)
  UA_TYPES_UINT32 = 6;

(**
 * Int64
 * ^^^^^
 *)
  UA_TYPES_INT64 = 7;

(**
 * UInt64
 * ^^^^^^
 *)
  UA_TYPES_UINT64 = 8;

(**
 * Float
 * ^^^^^
 *)
  UA_TYPES_FLOAT = 9;

(**
 * Double
 * ^^^^^^
 *)
  UA_TYPES_DOUBLE = 10;

(**
 * String
 * ^^^^^^
 *)
  UA_TYPES_STRING = 11;

(**
 * DateTime
 * ^^^^^^^^
 *)
  UA_TYPES_DATETIME = 12;

(**
 * Guid
 * ^^^^
 *)
  UA_TYPES_GUID = 13;

(**
 * ByteString
 * ^^^^^^^^^^
 *)
  UA_TYPES_BYTESTRING = 14;

(**
 * XmlElement
 * ^^^^^^^^^^
 *)
  UA_TYPES_XMLELEMENT = 15;

(**
 * NodeId
 * ^^^^^^
 *)
  UA_TYPES_NODEID = 16;

(**
 * ExpandedNodeId
 * ^^^^^^^^^^^^^^
 *)
  UA_TYPES_EXPANDEDNODEID = 17;

(**
 * StatusCode
 * ^^^^^^^^^^
 *)
  UA_TYPES_STATUSCODE = 18;

(**
 * QualifiedName
 * ^^^^^^^^^^^^^
 *)
  UA_TYPES_QUALIFIEDNAME = 19;

(**
 * LocalizedText
 * ^^^^^^^^^^^^^
 *)
  UA_TYPES_LOCALIZEDTEXT = 20;

(**
 * ExtensionObject
 * ^^^^^^^^^^^^^^^
 *)
  UA_TYPES_EXTENSIONOBJECT = 21;

(**
 * DataValue
 * ^^^^^^^^^
 *)
  UA_TYPES_DATAVALUE = 22;

(**
 * Variant
 * ^^^^^^^
 *)
  UA_TYPES_VARIANT = 23;

(**
 * DiagnosticInfo
 * ^^^^^^^^^^^^^^
 *)
  UA_TYPES_DIAGNOSTICINFO = 24;

(**
 * NodeClass
 * ^^^^^^^^^
 * A mask specifying the class of the node. *)
type
  UA_NodeClass = (
    UA_NODECLASS_UNSPECIFIED = 0,
    UA_NODECLASS_OBJECT = 1,
    UA_NODECLASS_VARIABLE = 2,
    UA_NODECLASS_METHOD = 4,
    UA_NODECLASS_OBJECTTYPE = 8,
    UA_NODECLASS_VARIABLETYPE = 16,
    UA_NODECLASS_REFERENCETYPE = 32,
    UA_NODECLASS_DATATYPE = 64,
    UA_NODECLASS_VIEW = 128,
    __UA_NODECLASS_FORCE32BIT = $7fffffff
  );
  PUA_NodeClass = ^UA_NodeClass;
  {$IF sizeof(UA_NodeClass) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_NODECLASS = 25;

(**
 * StructureType
 * ^^^^^^^^^^^^^
 *)
type
  UA_StructureType = (
    UA_STRUCTURETYPE_STRUCTURE = 0,
    UA_STRUCTURETYPE_STRUCTUREWITHOPTIONALFIELDS = 1,
    UA_STRUCTURETYPE_UNION = 2,
    __UA_STRUCTURETYPE_FORCE32BIT = $7fffffff
  );
  PUA_StructureType = ^UA_StructureType;
  {$IF sizeof(UA_StructureType) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_STRUCTURETYPE = 26;

(**
 * StructureField
 * ^^^^^^^^^^^^^^
 *)
type
  UA_StructureField = record
    name: UA_String;
    description: UA_LocalizedText;
    dataType: UA_NodeId;
    valueRank: UA_Int32;
    arrayDimensionsSize: Size_T;
    arrayDimensions: PUA_UInt32;
    maxStringLength: UA_UInt32;
    isOptional: UA_Boolean;
  end;
  PUA_StructureField = ^UA_StructureField;

const
  UA_TYPES_STRUCTUREFIELD = 27;

(**
 * StructureDefinition
 * ^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_StructureDefinition = record
    defaultEncodingId: UA_NodeId;
    baseDataType: UA_NodeId;
    structureType: UA_StructureType;
    fieldsSize: Size_T;
    fields: PUA_StructureField;
  end;
  PUA_StructureDefinition = ^UA_StructureDefinition;

const
  UA_TYPES_STRUCTUREDEFINITION = 28;

(**
 * Argument
 * ^^^^^^^^
 * An argument for a method. *)
type
  UA_Argument = record
    name: UA_String;
    dataType: UA_NodeId;
    valueRank: UA_Int32;
    arrayDimensionsSize: Size_T;
    arrayDimensions: PUA_UInt32;
    description: UA_LocalizedText;
  end;
  PUA_Argument = ^UA_Argument;

const
  UA_TYPES_ARGUMENT = 29;

(**
 * EnumValueType
 * ^^^^^^^^^^^^^
 * A mapping between a value of an enumerated type and a name and description. *)
type
  UA_EnumValueType = record
    value: UA_Int64;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
  end;
  PUA_EnumValueType = ^UA_EnumValueType;

const
  UA_TYPES_ENUMVALUETYPE = 30;

(**
 * EnumField
 * ^^^^^^^^^
 *)
type
  UA_EnumField = record
    value: UA_Int64;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    name: UA_String;
  end;
  PUA_EnumField = ^UA_EnumField;

const
  UA_TYPES_ENUMFIELD = 31;

(**
 * Duration
 * ^^^^^^^^
 * A period of time measured in milliseconds. *)
type
  UA_Duration = UA_Double;

const
  UA_TYPES_DURATION = 32;

(**
 * UtcTime
 * ^^^^^^^
 * A date/time value specified in Universal Coordinated Time (UTC). *)
type
  UA_UtcTime = UA_DateTime;

const
  UA_TYPES_UTCTIME = 33;

(**
 * LocaleId
 * ^^^^^^^^
 * An identifier for a user locale. *)
type
  UA_LocaleId = UA_String;

const
  UA_TYPES_LOCALEID = 34;

(**
 * ApplicationType
 * ^^^^^^^^^^^^^^^
 * The types of applications. *)
type
  UA_ApplicationType = (
    UA_APPLICATIONTYPE_SERVER = 0,
    UA_APPLICATIONTYPE_CLIENT = 1,
    UA_APPLICATIONTYPE_CLIENTANDSERVER = 2,
    UA_APPLICATIONTYPE_DISCOVERYSERVER = 3,
    __UA_APPLICATIONTYPE_FORCE32BIT = $7fffffff
  );
  PUA_ApplicationType = ^UA_ApplicationType;
  {$IF sizeof(UA_ApplicationType) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_APPLICATIONTYPE = 35;

(**
 * ApplicationDescription
 * ^^^^^^^^^^^^^^^^^^^^^^
 * Describes an application and how to find it. *)
type
  UA_ApplicationDescription = record
    applicationUri: UA_String;
    productUri: UA_String;
    applicationName: UA_LocalizedText;
    applicationType: UA_ApplicationType;
    gatewayServerUri: UA_String;
    discoveryProfileUri: UA_String;
    discoveryUrlsSize: Size_T;
    discoveryUrls: PUA_String;
  end;
  PUA_ApplicationDescription = ^UA_ApplicationDescription;

const
  UA_TYPES_APPLICATIONDESCRIPTION = 36;

(**
 * RequestHeader
 * ^^^^^^^^^^^^^
 * The header passed with every server request. *)
type
  UA_RequestHeader = record
    authenticationToken: UA_NodeId;
    timestamp: UA_DateTime;
    requestHandle: UA_UInt32;
    returnDiagnostics: UA_UInt32;
    auditEntryId: UA_String;
    timeoutHint: UA_UInt32;
    additionalHeader: UA_ExtensionObject;
  end;
  PUA_RequestHeader = ^UA_RequestHeader;

const
  UA_TYPES_REQUESTHEADER = 37;

(**
 * ResponseHeader
 * ^^^^^^^^^^^^^^
 * The header passed with every server response. *)
type
  UA_ResponseHeader = record
    timestamp: UA_DateTime;
    requestHandle: UA_UInt32;
    serviceResult: UA_StatusCode;
    serviceDiagnostics: UA_DiagnosticInfo;
    stringTableSize: Size_T;
    stringTable: PUA_String;
    additionalHeader: UA_ExtensionObject;
  end;
  PUA_ResponseHeader = ^UA_ResponseHeader;

const
  UA_TYPES_RESPONSEHEADER = 38;

(**
 * ServiceFault
 * ^^^^^^^^^^^^
 * The response returned by all services when there is a service level error. *)
type
  UA_ServiceFault = record
    responseHeader: UA_ResponseHeader;
  end;
  PUA_ServiceFault = ^UA_ServiceFault;

const
  UA_TYPES_SERVICEFAULT = 39;

(**
 * FindServersRequest
 * ^^^^^^^^^^^^^^^^^^
 * Finds the servers known to the discovery server. *)
type
  UA_FindServersRequest = record
    requestHeader: UA_RequestHeader;
    endpointUrl: UA_String;
    localeIdsSize: Size_T;
    localeIds: PUA_String;
    serverUrisSize: Size_T;
    serverUris: PUA_String;
  end;
  PUA_FindServersRequest = ^UA_FindServersRequest;

const
  UA_TYPES_FINDSERVERSREQUEST = 40;

(**
 * FindServersResponse
 * ^^^^^^^^^^^^^^^^^^^
 * Finds the servers known to the discovery server. *)
type
  UA_FindServersResponse = record
    responseHeader: UA_ResponseHeader;
    serversSize: Size_T;
    servers: PUA_ApplicationDescription;
  end;
  PUA_FindServersResponse = ^UA_FindServersResponse;

const
  UA_TYPES_FINDSERVERSRESPONSE = 41;

(**
 * MessageSecurityMode
 * ^^^^^^^^^^^^^^^^^^^
 * The type of security to use on a message. *)
type
  UA_MessageSecurityMode = (
    UA_MESSAGESECURITYMODE_INVALID = 0,
    UA_MESSAGESECURITYMODE_NONE = 1,
    UA_MESSAGESECURITYMODE_SIGN = 2,
    UA_MESSAGESECURITYMODE_SIGNANDENCRYPT = 3,
    __UA_MESSAGESECURITYMODE_FORCE32BIT = $7fffffff
  );
  PUA_MessageSecurityMode = ^UA_MessageSecurityMode;
  {$IF sizeof(UA_MessageSecurityMode) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_MESSAGESECURITYMODE = 42;

(**
 * UserTokenType
 * ^^^^^^^^^^^^^
 * The possible user token types. *)
type
  UA_UserTokenType = (
    UA_USERTOKENTYPE_ANONYMOUS = 0,
    UA_USERTOKENTYPE_USERNAME = 1,
    UA_USERTOKENTYPE_CERTIFICATE = 2,
    UA_USERTOKENTYPE_ISSUEDTOKEN = 3,
    __UA_USERTOKENTYPE_FORCE32BIT = $7fffffff
  );
  PUA_UserTokenType = ^UA_UserTokenType;
  {$IF sizeof(UA_UserTokenType) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_USERTOKENTYPE = 43;

(**
 * UserTokenPolicy
 * ^^^^^^^^^^^^^^^
 * Describes a user token that can be used with a server. *)
type
  UA_UserTokenPolicy = record
    policyId: UA_String;
    tokenType: UA_UserTokenType;
    issuedTokenType: UA_String;
    issuerEndpointUrl: UA_String;
    securityPolicyUri: UA_String;
  end;
  PUA_UserTokenPolicy = ^UA_UserTokenPolicy;

const
  UA_TYPES_USERTOKENPOLICY = 44;

(**
 * EndpointDescription
 * ^^^^^^^^^^^^^^^^^^^
 * The description of a endpoint that can be used to access a server. *)
type
  UA_EndpointDescription = record
    endpointUrl: UA_String;
    server: UA_ApplicationDescription;
    serverCertificate: UA_ByteString;
    securityMode: UA_MessageSecurityMode;
    securityPolicyUri: UA_String;
    userIdentityTokensSize: Size_T;
    userIdentityTokens: PUA_UserTokenPolicy;
    transportProfileUri: UA_String;
    securityLevel: UA_Byte;
  end;
  PUA_EndpointDescription = ^UA_EndpointDescription;

const
  UA_TYPES_ENDPOINTDESCRIPTION = 45;

(**
 * GetEndpointsRequest
 * ^^^^^^^^^^^^^^^^^^^
 * Gets the endpoints used by the server. *)
type
  UA_GetEndpointsRequest = record
    requestHeader: UA_RequestHeader;
    endpointUrl: UA_String;
    localeIdsSize: Size_T;
    localeIds: PUA_String;
    profileUrisSize: Size_T;
    profileUris: PUA_String;
  end;
  PUA_GetEndpointsRequest = ^UA_GetEndpointsRequest;

const
  UA_TYPES_GETENDPOINTSREQUEST = 46;

(**
 * GetEndpointsResponse
 * ^^^^^^^^^^^^^^^^^^^^
 * Gets the endpoints used by the server. *)
type
  UA_GetEndpointsResponse = record
    responseHeader: UA_ResponseHeader;
    endpointsSize: Size_T;
    endpoints: PUA_EndpointDescription;
  end;
  PUA_GetEndpointsResponse = ^UA_GetEndpointsResponse;

const
  UA_TYPES_GETENDPOINTSRESPONSE = 47;

(**
 * SecurityTokenRequestType
 * ^^^^^^^^^^^^^^^^^^^^^^^^
 * Indicates whether a token if being created or renewed. *)
type
  UA_SecurityTokenRequestType = (
    UA_SECURITYTOKENREQUESTTYPE_ISSUE = 0,
    UA_SECURITYTOKENREQUESTTYPE_RENEW = 1,
    __UA_SECURITYTOKENREQUESTTYPE_FORCE32BIT = $7fffffff
  );
  PUA_SecurityTokenRequestType = ^UA_SecurityTokenRequestType;
  {$IF sizeof(UA_SecurityTokenRequestType) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_SECURITYTOKENREQUESTTYPE = 48;

(**
 * ChannelSecurityToken
 * ^^^^^^^^^^^^^^^^^^^^
 * The token that identifies a set of keys for an active secure channel. *)
type
  UA_ChannelSecurityToken = record
    channelId: UA_UInt32;
    tokenId: UA_UInt32;
    createdAt: UA_DateTime;
    revisedLifetime: UA_UInt32;
  end;
  PUA_ChannelSecurityToken = ^UA_ChannelSecurityToken;

const
  UA_TYPES_CHANNELSECURITYTOKEN = 49;

(**
 * OpenSecureChannelRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^
 * Creates a secure channel with a server. *)
type
  UA_OpenSecureChannelRequest = record
    requestHeader: UA_RequestHeader;
    clientProtocolVersion: UA_UInt32;
    requestType: UA_SecurityTokenRequestType;
    securityMode: UA_MessageSecurityMode;
    clientNonce: UA_ByteString;
    requestedLifetime: UA_UInt32;
  end;
  PUA_OpenSecureChannelRequest = ^UA_OpenSecureChannelRequest;

const
  UA_TYPES_OPENSECURECHANNELREQUEST = 50;

(**
 * OpenSecureChannelResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 * Creates a secure channel with a server. *)
type
  UA_OpenSecureChannelResponse = record
    responseHeader: UA_ResponseHeader;
    serverProtocolVersion: UA_UInt32;
    securityToken: UA_ChannelSecurityToken;
    serverNonce: UA_ByteString;
  end;
  PUA_OpenSecureChannelResponse = ^UA_OpenSecureChannelResponse;

const
  UA_TYPES_OPENSECURECHANNELRESPONSE = 51;

(**
 * CloseSecureChannelRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 * Closes a secure channel. *)
type
  UA_CloseSecureChannelRequest = record
    requestHeader: UA_RequestHeader;
  end;
  PUA_CloseSecureChannelRequest = ^UA_CloseSecureChannelRequest;

const
  UA_TYPES_CLOSESECURECHANNELREQUEST = 52;

(**
 * CloseSecureChannelResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 * Closes a secure channel. *)
type
  UA_CloseSecureChannelResponse = record
    responseHeader: UA_ResponseHeader;
  end;
  PUA_CloseSecureChannelResponse = ^UA_CloseSecureChannelResponse;

const
  UA_TYPES_CLOSESECURECHANNELRESPONSE = 53;

(**
 * SignedSoftwareCertificate
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 * A software certificate with a digital signature. *)
type
  UA_SignedSoftwareCertificate = record
    certificateData: UA_ByteString;
    signature: UA_ByteString;
  end;
  PUA_SignedSoftwareCertificate = ^UA_SignedSoftwareCertificate;

const
  UA_TYPES_SIGNEDSOFTWARECERTIFICATE = 54;

(**
 * SignatureData
 * ^^^^^^^^^^^^^
 * A digital signature. *)
type
  UA_SignatureData = record
    algorithm: UA_String;
    signature: UA_ByteString;
  end;
  PUA_SignatureData = ^UA_SignatureData;

const
  UA_TYPES_SIGNATUREDATA = 55;

(**
 * CreateSessionRequest
 * ^^^^^^^^^^^^^^^^^^^^
 * Creates a new session with the server. *)
type
  UA_CreateSessionRequest = record
    requestHeader: UA_RequestHeader;
    clientDescription: UA_ApplicationDescription;
    serverUri: UA_String;
    endpointUrl: UA_String;
    sessionName: UA_String;
    clientNonce: UA_ByteString;
    clientCertificate: UA_ByteString;
    requestedSessionTimeout: UA_Double;
    maxResponseMessageSize: UA_UInt32;
  end;
  PUA_CreateSessionRequest = ^UA_CreateSessionRequest;

const
  UA_TYPES_CREATESESSIONREQUEST = 56;

(**
 * CreateSessionResponse
 * ^^^^^^^^^^^^^^^^^^^^^
 * Creates a new session with the server. *)
type
  UA_CreateSessionResponse = record
    responseHeader: UA_ResponseHeader;
    sessionId: UA_NodeId;
    authenticationToken: UA_NodeId;
    revisedSessionTimeout: UA_Double;
    serverNonce: UA_ByteString;
    serverCertificate: UA_ByteString;
    serverEndpointsSize: Size_T;
    serverEndpoints: PUA_EndpointDescription;
    serverSoftwareCertificatesSize: Size_T;
    serverSoftwareCertificates: PUA_SignedSoftwareCertificate;
    serverSignature: UA_SignatureData;
    maxRequestMessageSize: UA_UInt32;
  end;
  PUA_CreateSessionResponse = ^UA_CreateSessionResponse;

const
  UA_TYPES_CREATESESSIONRESPONSE = 57;

(**
 * UserIdentityToken
 * ^^^^^^^^^^^^^^^^^
 * A base type for a user identity token. *)
type
  UA_UserIdentityToken = record
    policyId: UA_String;
  end;
  PUA_UserIdentityToken = ^UA_UserIdentityToken;

const
  UA_TYPES_USERIDENTITYTOKEN = 58;

(**
 * AnonymousIdentityToken
 * ^^^^^^^^^^^^^^^^^^^^^^
 * A token representing an anonymous user. *)
type
  UA_AnonymousIdentityToken = record
    policyId: UA_String;
  end;
  PUA_AnonymousIdentityToken = ^UA_AnonymousIdentityToken;

const
  UA_TYPES_ANONYMOUSIDENTITYTOKEN = 59;

(**
 * UserNameIdentityToken
 * ^^^^^^^^^^^^^^^^^^^^^
 * A token representing a user identified by a user name and password. *)
type
  UA_UserNameIdentityToken = record
    policyId: UA_String;
    userName: UA_String;
    password: UA_ByteString;
    encryptionAlgorithm: UA_String;
  end;
  PUA_UserNameIdentityToken = ^UA_UserNameIdentityToken;

const
  UA_TYPES_USERNAMEIDENTITYTOKEN = 60;

(**
 * X509IdentityToken
 * ^^^^^^^^^^^^^^^^^
 * A token representing a user identified by an X509 certificate. *)
type
  UA_X509IdentityToken = record
    policyId: UA_String;
    certificateData: UA_ByteString;
  end;
  PUA_X509IdentityToken = ^UA_X509IdentityToken;

const
  UA_TYPES_X509IDENTITYTOKEN = 61;

(**
 * IssuedIdentityToken
 * ^^^^^^^^^^^^^^^^^^^
 * A token representing a user identified by a WS-Security XML token. *)
type
  UA_IssuedIdentityToken = record
    policyId: UA_String;
    tokenData: UA_ByteString;
    encryptionAlgorithm: UA_String;
  end;
  PUA_IssuedIdentityToken = ^UA_IssuedIdentityToken;

const
  UA_TYPES_ISSUEDIDENTITYTOKEN = 62;

(**
 * ActivateSessionRequest
 * ^^^^^^^^^^^^^^^^^^^^^^
 * Activates a session with the server. *)
type
  UA_ActivateSessionRequest = record
    requestHeader: UA_RequestHeader;
    clientSignature: UA_SignatureData;
    clientSoftwareCertificatesSize: Size_T;
    clientSoftwareCertificates: PUA_SignedSoftwareCertificate;
    localeIdsSize: Size_T;
    localeIds: PUA_String;
    userIdentityToken: UA_ExtensionObject;
    userTokenSignature: UA_SignatureData;
  end;
  PUA_ActivateSessionRequest = ^UA_ActivateSessionRequest;

const
  UA_TYPES_ACTIVATESESSIONREQUEST = 63;

(**
 * ActivateSessionResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^
 * Activates a session with the server. *)
type
  UA_ActivateSessionResponse = record
    responseHeader: UA_ResponseHeader;
    serverNonce: UA_ByteString;
    resultsSize: Size_T;
    results: PUA_StatusCode;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_ActivateSessionResponse = ^UA_ActivateSessionResponse;

const
  UA_TYPES_ACTIVATESESSIONRESPONSE = 64;

(**
 * CloseSessionRequest
 * ^^^^^^^^^^^^^^^^^^^
 * Closes a session with the server. *)
type
  UA_CloseSessionRequest = record
    requestHeader: UA_RequestHeader;
    deleteSubscriptions: UA_Boolean;
  end;
  PUA_CloseSessionRequest = ^UA_CloseSessionRequest;

const
  UA_TYPES_CLOSESESSIONREQUEST = 65;

(**
 * CloseSessionResponse
 * ^^^^^^^^^^^^^^^^^^^^
 * Closes a session with the server. *)
type
  UA_CloseSessionResponse = record
    responseHeader: UA_ResponseHeader;
  end;
  PUA_CloseSessionResponse = ^UA_CloseSessionResponse;

const
  UA_TYPES_CLOSESESSIONRESPONSE = 66;

(**
 * NodeAttributesMask
 * ^^^^^^^^^^^^^^^^^^
 * The bits used to specify default attributes for a new node. *)
type
  UA_NodeAttributesMask = (
    UA_NODEATTRIBUTESMASK_NONE = 0,
    UA_NODEATTRIBUTESMASK_ACCESSLEVEL = 1,
    UA_NODEATTRIBUTESMASK_ARRAYDIMENSIONS = 2,
    UA_NODEATTRIBUTESMASK_BROWSENAME = 4,
    UA_NODEATTRIBUTESMASK_CONTAINSNOLOOPS = 8,
    UA_NODEATTRIBUTESMASK_DATATYPE = 16,
    UA_NODEATTRIBUTESMASK_DESCRIPTION = 32,
    UA_NODEATTRIBUTESMASK_DISPLAYNAME = 64,
    UA_NODEATTRIBUTESMASK_EVENTNOTIFIER = 128,
    UA_NODEATTRIBUTESMASK_EXECUTABLE = 256,
    UA_NODEATTRIBUTESMASK_HISTORIZING = 512,
    UA_NODEATTRIBUTESMASK_INVERSENAME = 1024,
    UA_NODEATTRIBUTESMASK_ISABSTRACT = 2048,
    UA_NODEATTRIBUTESMASK_MINIMUMSAMPLINGINTERVAL = 4096,
    UA_NODEATTRIBUTESMASK_NODECLASS = 8192,
    UA_NODEATTRIBUTESMASK_NODEID = 16384,
    UA_NODEATTRIBUTESMASK_SYMMETRIC = 32768,
    UA_NODEATTRIBUTESMASK_USERACCESSLEVEL = 65536,
    UA_NODEATTRIBUTESMASK_USEREXECUTABLE = 131072,
    UA_NODEATTRIBUTESMASK_USERWRITEMASK = 262144,
    UA_NODEATTRIBUTESMASK_VALUERANK = 524288,
    UA_NODEATTRIBUTESMASK_WRITEMASK = 1048576,
    UA_NODEATTRIBUTESMASK_VALUE = 2097152,
    UA_NODEATTRIBUTESMASK_DATATYPEDEFINITION = 4194304,
    UA_NODEATTRIBUTESMASK_ROLEPERMISSIONS = 8388608,
    UA_NODEATTRIBUTESMASK_ACCESSRESTRICTIONS = 16777216,
    UA_NODEATTRIBUTESMASK_ALL = 33554431,
    UA_NODEATTRIBUTESMASK_BASENODE = 26501220,
    UA_NODEATTRIBUTESMASK_OBJECT = 26501348,
    UA_NODEATTRIBUTESMASK_OBJECTTYPE = 26503268,
    UA_NODEATTRIBUTESMASK_VARIABLE = 26571383,
    UA_NODEATTRIBUTESMASK_VARIABLETYPE = 28600438,
    UA_NODEATTRIBUTESMASK_METHOD = 26632548,
    UA_NODEATTRIBUTESMASK_REFERENCETYPE = 26537060,
    UA_NODEATTRIBUTESMASK_VIEW = 26501356,
    __UA_NODEATTRIBUTESMASK_FORCE32BIT = $7fffffff
  );
  PUA_NodeAttributesMask = ^UA_NodeAttributesMask;
  {$IF sizeof(UA_NodeAttributesMask) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_NODEATTRIBUTESMASK = 67;

(**
 * NodeAttributes
 * ^^^^^^^^^^^^^^
 * The base attributes for all nodes. *)
type
  UA_NodeAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
  end;
  PUA_NodeAttributes = ^UA_NodeAttributes;

const
  UA_TYPES_NODEATTRIBUTES = 68;

(**
 * ObjectAttributes
 * ^^^^^^^^^^^^^^^^
 * The attributes for an object node. *)
type
  UA_ObjectAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    eventNotifier: UA_Byte;
  end;
  PUA_ObjectAttributes = ^UA_ObjectAttributes;

const
  UA_TYPES_OBJECTATTRIBUTES = 69;

(**
 * VariableAttributes
 * ^^^^^^^^^^^^^^^^^^
 * The attributes for a variable node. *)
type
  UA_VariableAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    value: UA_Variant;
    dataType: UA_NodeId;
    valueRank: UA_Int32;
    arrayDimensionsSize: Size_T;
    arrayDimensions: PUA_UInt32;
    accessLevel: UA_Byte;
    userAccessLevel: UA_Byte;
    minimumSamplingInterval: UA_Double;
    historizing: UA_Boolean;
  end;
  PUA_VariableAttributes = ^UA_VariableAttributes;

const
  UA_TYPES_VARIABLEATTRIBUTES = 70;

(**
 * MethodAttributes
 * ^^^^^^^^^^^^^^^^
 * The attributes for a method node. *)
type
  UA_MethodAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    executable: UA_Boolean;
    userExecutable: UA_Boolean;
  end;
  PUA_MethodAttributes = ^UA_MethodAttributes;

const
  UA_TYPES_METHODATTRIBUTES = 71;

(**
 * ObjectTypeAttributes
 * ^^^^^^^^^^^^^^^^^^^^
 * The attributes for an object type node. *)
type
  UA_ObjectTypeAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    isAbstract: UA_Boolean;
  end;
  PUA_ObjectTypeAttributes = ^UA_ObjectTypeAttributes;

const
  UA_TYPES_OBJECTTYPEATTRIBUTES = 72;

(**
 * VariableTypeAttributes
 * ^^^^^^^^^^^^^^^^^^^^^^
 * The attributes for a variable type node. *)
type
  UA_VariableTypeAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    value: UA_Variant;
    dataType: UA_NodeId;
    valueRank: UA_Int32;
    arrayDimensionsSize: Size_T;
    arrayDimensions: PUA_UInt32;
    isAbstract: UA_Boolean;
  end;
  PUA_VariableTypeAttributes = ^UA_VariableTypeAttributes;

const
  UA_TYPES_VARIABLETYPEATTRIBUTES = 73;

(**
 * ReferenceTypeAttributes
 * ^^^^^^^^^^^^^^^^^^^^^^^
 * The attributes for a reference type node. *)
type
  UA_ReferenceTypeAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    isAbstract: UA_Boolean;
    symmetric: UA_Boolean;
    inverseName: UA_LocalizedText;
  end;
  PUA_ReferenceTypeAttributes = ^UA_ReferenceTypeAttributes;

const
  UA_TYPES_REFERENCETYPEATTRIBUTES = 74;

(**
 * DataTypeAttributes
 * ^^^^^^^^^^^^^^^^^^
 * The attributes for a data type node. *)
type
  UA_DataTypeAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    isAbstract: UA_Boolean;
  end;
  PUA_DataTypeAttributes = ^UA_DataTypeAttributes;

const
  UA_TYPES_DATATYPEATTRIBUTES = 75;

(**
 * ViewAttributes
 * ^^^^^^^^^^^^^^
 * The attributes for a view node. *)
type
  UA_ViewAttributes = record
    specifiedAttributes: UA_UInt32;
    displayName: UA_LocalizedText;
    description: UA_LocalizedText;
    writeMask: UA_UInt32;
    userWriteMask: UA_UInt32;
    containsNoLoops: UA_Boolean;
    eventNotifier: UA_Byte;
  end;
  PUA_ViewAttributes = ^UA_ViewAttributes;

const
  UA_TYPES_VIEWATTRIBUTES = 76;

(**
 * AddNodesItem
 * ^^^^^^^^^^^^
 * A request to add a node to the server address space. *)
type
  UA_AddNodesItem = record
    parentNodeId: UA_ExpandedNodeId;
    referenceTypeId: UA_NodeId;
    requestedNewNodeId: UA_ExpandedNodeId;
    browseName: UA_QualifiedName;
    nodeClass: UA_NodeClass;
    nodeAttributes: UA_ExtensionObject;
    typeDefinition: UA_ExpandedNodeId;
  end;
  PUA_AddNodesItem = ^UA_AddNodesItem;

const
  UA_TYPES_ADDNODESITEM = 77;

(**
 * AddNodesResult
 * ^^^^^^^^^^^^^^
 * A result of an add node operation. *)
type
  UA_AddNodesResult = record
    statusCode: UA_StatusCode;
    addedNodeId: UA_NodeId;
  end;
  PUA_AddNodesResult = ^UA_AddNodesResult;

const
  UA_TYPES_ADDNODESRESULT = 78;

(**
 * AddNodesRequest
 * ^^^^^^^^^^^^^^^
 * Adds one or more nodes to the server address space. *)
type
  UA_AddNodesRequest = record
    requestHeader: UA_RequestHeader;
    nodesToAddSize: Size_T;
    nodesToAdd: PUA_AddNodesItem;
  end;
  PUA_AddNodesRequest = ^UA_AddNodesRequest;

const
  UA_TYPES_ADDNODESREQUEST = 79;

(**
 * AddNodesResponse
 * ^^^^^^^^^^^^^^^^
 * Adds one or more nodes to the server address space. *)
type
  UA_AddNodesResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_AddNodesResult;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_AddNodesResponse = ^UA_AddNodesResponse;

const
  UA_TYPES_ADDNODESRESPONSE = 80;

(**
 * AddReferencesItem
 * ^^^^^^^^^^^^^^^^^
 * A request to add a reference to the server address space. *)
type
  UA_AddReferencesItem = record
    sourceNodeId: UA_NodeId;
    referenceTypeId: UA_NodeId;
    isForward: UA_Boolean;
    targetServerUri: UA_String;
    targetNodeId: UA_ExpandedNodeId;
    targetNodeClass: UA_NodeClass;
  end;
  PUA_AddReferencesItem = ^UA_AddReferencesItem;

const
  UA_TYPES_ADDREFERENCESITEM = 81;

(**
 * AddReferencesRequest
 * ^^^^^^^^^^^^^^^^^^^^
 * Adds one or more references to the server address space. *)
type
  UA_AddReferencesRequest = record
    requestHeader: UA_RequestHeader;
    referencesToAddSize: Size_T;
    referencesToAdd: PUA_AddReferencesItem;
  end;
  PUA_AddReferencesRequest = ^UA_AddReferencesRequest;

const
  UA_TYPES_ADDREFERENCESREQUEST = 82;

(**
 * AddReferencesResponse
 * ^^^^^^^^^^^^^^^^^^^^^
 * Adds one or more references to the server address space. *)
type
  UA_AddReferencesResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_StatusCode;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_AddReferencesResponse = ^UA_AddReferencesResponse;

const
  UA_TYPES_ADDREFERENCESRESPONSE = 83;

(**
 * DeleteNodesItem
 * ^^^^^^^^^^^^^^^
 * A request to delete a node to the server address space. *)
type
  UA_DeleteNodesItem = record
    nodeId: UA_NodeId;
    deleteTargetReferences: UA_Boolean;
  end;
  PUA_DeleteNodesItem = ^UA_DeleteNodesItem;

const
  UA_TYPES_DELETENODESITEM = 84;

(**
 * DeleteNodesRequest
 * ^^^^^^^^^^^^^^^^^^
 * Delete one or more nodes from the server address space. *)
type
  UA_DeleteNodesRequest = record
    requestHeader: UA_RequestHeader;
    nodesToDeleteSize: Size_T;
    nodesToDelete: PUA_DeleteNodesItem;
  end;
  PUA_DeleteNodesRequest = ^UA_DeleteNodesRequest;

const
  UA_TYPES_DELETENODESREQUEST = 85;

(**
 * DeleteNodesResponse
 * ^^^^^^^^^^^^^^^^^^^
 * Delete one or more nodes from the server address space. *)
type
  UA_DeleteNodesResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_StatusCode;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_DeleteNodesResponse = ^UA_DeleteNodesResponse;

const
  UA_TYPES_DELETENODESRESPONSE = 86;

(**
 * DeleteReferencesItem
 * ^^^^^^^^^^^^^^^^^^^^
 * A request to delete a node from the server address space. *)
type
  UA_DeleteReferencesItem = record
    sourceNodeId: UA_NodeId;
    referenceTypeId: UA_NodeId;
    isForward: UA_Boolean;
    targetNodeId: UA_ExpandedNodeId;
    deleteBidirectional: UA_Boolean;
  end;
  PUA_DeleteReferencesItem = ^UA_DeleteReferencesItem;

const
  UA_TYPES_DELETEREFERENCESITEM = 87;

(**
 * DeleteReferencesRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^
 * Delete one or more references from the server address space. *)
type
  UA_DeleteReferencesRequest = record
    requestHeader: UA_RequestHeader;
    referencesToDeleteSize: Size_T;
    referencesToDelete: PUA_DeleteReferencesItem;
  end;
  PUA_DeleteReferencesRequest = ^UA_DeleteReferencesRequest;

const
  UA_TYPES_DELETEREFERENCESREQUEST = 88;

(**
 * DeleteReferencesResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^
 * Delete one or more references from the server address space. *)
type
  UA_DeleteReferencesResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_StatusCode;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_DeleteReferencesResponse = ^UA_DeleteReferencesResponse;

const
  UA_TYPES_DELETEREFERENCESRESPONSE = 89;

(**
 * BrowseDirection
 * ^^^^^^^^^^^^^^^
 * The directions of the references to return. *)
type
  UA_BrowseDirection = (
    UA_BROWSEDIRECTION_FORWARD = 0,
    UA_BROWSEDIRECTION_INVERSE = 1,
    UA_BROWSEDIRECTION_BOTH = 2,
    UA_BROWSEDIRECTION_INVALID = 3,
    __UA_BROWSEDIRECTION_FORCE32BIT = $7fffffff
  );
  PUA_BrowseDirection = ^UA_BrowseDirection;
  {$IF sizeof(UA_BrowseDirection) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_BROWSEDIRECTION = 90;

(**
 * ViewDescription
 * ^^^^^^^^^^^^^^^
 * The view to browse. *)
type
  UA_ViewDescription = record
    viewId: UA_NodeId;
    timestamp: UA_DateTime;
    viewVersion: UA_UInt32;
  end;
  PUA_ViewDescription = ^UA_ViewDescription;

const
  UA_TYPES_VIEWDESCRIPTION = 91;

(**
 * BrowseDescription
 * ^^^^^^^^^^^^^^^^^
 * A request to browse the the references from a node. *)
type
  UA_BrowseDescription = record
    nodeId: UA_NodeId;
    browseDirection: UA_BrowseDirection;
    referenceTypeId: UA_NodeId;
    includeSubtypes: UA_Boolean;
    nodeClassMask: UA_UInt32;
    resultMask: UA_UInt32;
  end;
  PUA_BrowseDescription = ^UA_BrowseDescription;

const
  UA_TYPES_BROWSEDESCRIPTION = 92;

(**
 * BrowseResultMask
 * ^^^^^^^^^^^^^^^^
 * A bit mask which specifies what should be returned in a browse response. *)
type
  UA_BrowseResultMask = (
    UA_BROWSERESULTMASK_NONE = 0,
    UA_BROWSERESULTMASK_REFERENCETYPEID = 1,
    UA_BROWSERESULTMASK_ISFORWARD = 2,
    UA_BROWSERESULTMASK_NODECLASS = 4,
    UA_BROWSERESULTMASK_BROWSENAME = 8,
    UA_BROWSERESULTMASK_DISPLAYNAME = 16,
    UA_BROWSERESULTMASK_TYPEDEFINITION = 32,
    UA_BROWSERESULTMASK_ALL = 63,
    UA_BROWSERESULTMASK_REFERENCETYPEINFO = 3,
    UA_BROWSERESULTMASK_TARGETINFO = 60,
    __UA_BROWSERESULTMASK_FORCE32BIT = $7fffffff
  );
  PUA_BrowseResultMask = ^UA_BrowseResultMask;
  {$IF sizeof(UA_BrowseResultMask) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_BROWSERESULTMASK = 93;

(**
 * ReferenceDescription
 * ^^^^^^^^^^^^^^^^^^^^
 * The description of a reference. *)
type
  UA_ReferenceDescription = record
    referenceTypeId: UA_NodeId;
    isForward: UA_Boolean;
    nodeId: UA_ExpandedNodeId;
    browseName: UA_QualifiedName;
    displayName: UA_LocalizedText;
    nodeClass: UA_NodeClass;
    typeDefinition: UA_ExpandedNodeId;
  end;
  PUA_ReferenceDescription = ^UA_ReferenceDescription;

const
  UA_TYPES_REFERENCEDESCRIPTION = 94;

(**
 * BrowseResult
 * ^^^^^^^^^^^^
 * The result of a browse operation. *)
type
  UA_BrowseResult = record
    statusCode: UA_StatusCode;
    continuationPoint: UA_ByteString;
    referencesSize: Size_T;
    references: PUA_ReferenceDescription;
  end;
  PUA_BrowseResult = ^UA_BrowseResult;

const
  UA_TYPES_BROWSERESULT = 95;

(**
 * BrowseRequest
 * ^^^^^^^^^^^^^
 * Browse the references for one or more nodes from the server address space. *)
type
  UA_BrowseRequest = record
    requestHeader: UA_RequestHeader;
    view: UA_ViewDescription;
    requestedMaxReferencesPerNode: UA_UInt32;
    nodesToBrowseSize: Size_T;
    nodesToBrowse: PUA_BrowseDescription;
  end;
  PUA_BrowseRequest = ^UA_BrowseRequest;

const
  UA_TYPES_BROWSEREQUEST = 96;

(**
 * BrowseResponse
 * ^^^^^^^^^^^^^^
 * Browse the references for one or more nodes from the server address space. *)
type
  UA_BrowseResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_BrowseResult;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_BrowseResponse = ^UA_BrowseResponse;

const
  UA_TYPES_BROWSERESPONSE = 97;

(**
 * BrowseNextRequest
 * ^^^^^^^^^^^^^^^^^
 * Continues one or more browse operations. *)
type
  UA_BrowseNextRequest = record
    requestHeader: UA_RequestHeader;
    releaseContinuationPoints: UA_Boolean;
    continuationPointsSize: Size_T;
    continuationPoints: PUA_ByteString;
  end;
  PUA_BrowseNextRequest = ^UA_BrowseNextRequest;

const
  UA_TYPES_BROWSENEXTREQUEST = 98;

(**
 * BrowseNextResponse
 * ^^^^^^^^^^^^^^^^^^
 * Continues one or more browse operations. *)
type
  UA_BrowseNextResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_BrowseResult;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_BrowseNextResponse = ^UA_BrowseNextResponse;

const
  UA_TYPES_BROWSENEXTRESPONSE = 99;

(**
 * RelativePathElement
 * ^^^^^^^^^^^^^^^^^^^
 * An element in a relative path. *)
type
  UA_RelativePathElement = record
    referenceTypeId: UA_NodeId;
    isInverse: UA_Boolean;
    includeSubtypes: UA_Boolean;
    targetName: UA_QualifiedName;
  end;
  PUA_RelativePathElement = ^UA_RelativePathElement;

const
  UA_TYPES_RELATIVEPATHELEMENT = 100;

(**
 * RelativePath
 * ^^^^^^^^^^^^
 * A relative path constructed from reference types and browse names. *)
type
  UA_RelativePath = record
    elementsSize: Size_T;
    elements: PUA_RelativePathElement;
  end;
  PUA_RelativePath = ^UA_RelativePath;

const
  UA_TYPES_RELATIVEPATH = 101;

(**
 * BrowsePath
 * ^^^^^^^^^^
 * A request to translate a path into a node id. *)
type
  UA_BrowsePath = record
    startingNode: UA_NodeId;
    relativePath: UA_RelativePath;
  end;
  PUA_BrowsePath = ^UA_BrowsePath;

const
  UA_TYPES_BROWSEPATH = 102;

(**
 * BrowsePathTarget
 * ^^^^^^^^^^^^^^^^
 * The target of the translated path. *)
type
  UA_BrowsePathTarget = record
    targetId: UA_ExpandedNodeId;
    remainingPathIndex: UA_UInt32;
  end;
  PUA_BrowsePathTarget = ^UA_BrowsePathTarget;

const
  UA_TYPES_BROWSEPATHTARGET = 103;

(**
 * BrowsePathResult
 * ^^^^^^^^^^^^^^^^
 * The result of a translate opearation. *)
type
  UA_BrowsePathResult = record
    statusCode: UA_StatusCode;
    targetsSize: Size_T;
    targets: PUA_BrowsePathTarget;
  end;
  PUA_BrowsePathResult = ^UA_BrowsePathResult;

const
  UA_TYPES_BROWSEPATHRESULT = 104;

(**
 * TranslateBrowsePathsToNodeIdsRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 * Translates one or more paths in the server address space. *)
type
  UA_TranslateBrowsePathsToNodeIdsRequest = record
    requestHeader: UA_RequestHeader;
    browsePathsSize: Size_T;
    browsePaths: PUA_BrowsePath;
  end;
  PUA_TranslateBrowsePathsToNodeIdsRequest = ^UA_TranslateBrowsePathsToNodeIdsRequest;

const
  UA_TYPES_TRANSLATEBROWSEPATHSTONODEIDSREQUEST = 105;

(**
 * TranslateBrowsePathsToNodeIdsResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 * Translates one or more paths in the server address space. *)
type
  UA_TranslateBrowsePathsToNodeIdsResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_BrowsePathResult;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_TranslateBrowsePathsToNodeIdsResponse = ^UA_TranslateBrowsePathsToNodeIdsResponse;

const
  UA_TYPES_TRANSLATEBROWSEPATHSTONODEIDSRESPONSE = 106;

(**
 * RegisterNodesRequest
 * ^^^^^^^^^^^^^^^^^^^^
 * Registers one or more nodes for repeated use within a session. *)
type
  UA_RegisterNodesRequest = record
    requestHeader: UA_RequestHeader;
    nodesToRegisterSize: Size_T;
    nodesToRegister: PUA_NodeId;
  end;
  PUA_RegisterNodesRequest = ^UA_RegisterNodesRequest;

const
  UA_TYPES_REGISTERNODESREQUEST = 107;

(**
 * RegisterNodesResponse
 * ^^^^^^^^^^^^^^^^^^^^^
 * Registers one or more nodes for repeated use within a session. *)
type
  UA_RegisterNodesResponse = record
    responseHeader: UA_ResponseHeader;
    registeredNodeIdsSize: Size_T;
    registeredNodeIds: PUA_NodeId;
  end;
  PUA_RegisterNodesResponse = ^UA_RegisterNodesResponse;

const
  UA_TYPES_REGISTERNODESRESPONSE = 108;

(**
 * UnregisterNodesRequest
 * ^^^^^^^^^^^^^^^^^^^^^^
 * Unregisters one or more previously registered nodes. *)
type
  UA_UnregisterNodesRequest = record
    requestHeader: UA_RequestHeader;
    nodesToUnregisterSize: Size_T;
    nodesToUnregister: PUA_NodeId;
  end;
  PUA_UnregisterNodesRequest = ^UA_UnregisterNodesRequest;

const
  UA_TYPES_UNREGISTERNODESREQUEST = 109;

(**
 * UnregisterNodesResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^
 * Unregisters one or more previously registered nodes. *)
type
  UA_UnregisterNodesResponse = record
    responseHeader: UA_ResponseHeader;
  end;
  PUA_UnregisterNodesResponse = ^UA_UnregisterNodesResponse;

const
  UA_TYPES_UNREGISTERNODESRESPONSE = 110;

(**
 * FilterOperator
 * ^^^^^^^^^^^^^^
 *)
type
  UA_FilterOperator = (
    UA_FILTEROPERATOR_EQUALS = 0,
    UA_FILTEROPERATOR_ISNULL = 1,
    UA_FILTEROPERATOR_GREATERTHAN = 2,
    UA_FILTEROPERATOR_LESSTHAN = 3,
    UA_FILTEROPERATOR_GREATERTHANOREQUAL = 4,
    UA_FILTEROPERATOR_LESSTHANOREQUAL = 5,
    UA_FILTEROPERATOR_LIKE = 6,
    UA_FILTEROPERATOR_NOT = 7,
    UA_FILTEROPERATOR_BETWEEN = 8,
    UA_FILTEROPERATOR_INLIST = 9,
    UA_FILTEROPERATOR_AND = 10,
    UA_FILTEROPERATOR_OR = 11,
    UA_FILTEROPERATOR_CAST = 12,
    UA_FILTEROPERATOR_INVIEW = 13,
    UA_FILTEROPERATOR_OFTYPE = 14,
    UA_FILTEROPERATOR_RELATEDTO = 15,
    UA_FILTEROPERATOR_BITWISEAND = 16,
    UA_FILTEROPERATOR_BITWISEOR = 17,
    __UA_FILTEROPERATOR_FORCE32BIT = $7fffffff
  );
  PUA_FilterOperator = ^UA_FilterOperator;
  {$IF sizeof(UA_FilterOperator) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_FILTEROPERATOR = 111;

(**
 * ContentFilterElement
 * ^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ContentFilterElement = record
    filterOperator: UA_FilterOperator;
    filterOperandsSize: Size_T;
    filterOperands: PUA_ExtensionObject;
  end;
  PUA_ContentFilterElement = ^UA_ContentFilterElement;

const
  UA_TYPES_CONTENTFILTERELEMENT = 112;

(**
 * ContentFilter
 * ^^^^^^^^^^^^^
 *)
type
  UA_ContentFilter = record
    elementsSize: Size_T;
    elements: PUA_ContentFilterElement;
  end;
  PUA_ContentFilter = ^UA_ContentFilter;

const
  UA_TYPES_CONTENTFILTER = 113;

(**
 * FilterOperand
 * ^^^^^^^^^^^^^
 *)
type
  UA_FilterOperand = pointer;

const
  UA_TYPES_FILTEROPERAND = 114;

(**
 * ElementOperand
 * ^^^^^^^^^^^^^^
 *)
type
  UA_ElementOperand = record
    index: UA_UInt32;
  end;
  PUA_ElementOperand = ^UA_ElementOperand;

const
  UA_TYPES_ELEMENTOPERAND = 115;

(**
 * LiteralOperand
 * ^^^^^^^^^^^^^^
 *)
type
  UA_LiteralOperand = record
    value: UA_Variant;
  end;
  PUA_LiteralOperand = ^UA_LiteralOperand;

const
  UA_TYPES_LITERALOPERAND = 116;

(**
 * AttributeOperand
 * ^^^^^^^^^^^^^^^^
 *)
type
  UA_AttributeOperand = record
    nodeId: UA_NodeId;
    alias: UA_String;
    browsePath: UA_RelativePath;
    attributeId: UA_UInt32;
    indexRange: UA_String;
  end;
  PUA_AttributeOperand = ^UA_AttributeOperand;

const
  UA_TYPES_ATTRIBUTEOPERAND = 117;

(**
 * SimpleAttributeOperand
 * ^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SimpleAttributeOperand = record
    typeDefinitionId: UA_NodeId;
    browsePathSize: Size_T;
    browsePath: PUA_QualifiedName;
    attributeId: UA_UInt32;
    indexRange: UA_String;
  end;
  PUA_SimpleAttributeOperand = ^UA_SimpleAttributeOperand;

const
  UA_TYPES_SIMPLEATTRIBUTEOPERAND = 118;

(**
 * ContentFilterElementResult
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ContentFilterElementResult = record
    statusCode: UA_StatusCode;
    operandStatusCodesSize: Size_T;
    operandStatusCodes: PUA_StatusCode;
    operandDiagnosticInfosSize: Size_T;
    operandDiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_ContentFilterElementResult = ^UA_ContentFilterElementResult;

const
  UA_TYPES_CONTENTFILTERELEMENTRESULT = 119;

(**
 * ContentFilterResult
 * ^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ContentFilterResult = record
    elementResultsSize: Size_T;
    elementResults: PUA_ContentFilterElementResult;
    elementDiagnosticInfosSize: Size_T;
    elementDiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_ContentFilterResult = ^UA_ContentFilterResult;

const
  UA_TYPES_CONTENTFILTERRESULT = 120;

(**
 * TimestampsToReturn
 * ^^^^^^^^^^^^^^^^^^
 *)
type
  UA_TimestampsToReturn = (
    UA_TIMESTAMPSTORETURN_SOURCE = 0,
    UA_TIMESTAMPSTORETURN_SERVER = 1,
    UA_TIMESTAMPSTORETURN_BOTH = 2,
    UA_TIMESTAMPSTORETURN_NEITHER = 3,
    UA_TIMESTAMPSTORETURN_INVALID = 4,
    __UA_TIMESTAMPSTORETURN_FORCE32BIT = $7fffffff
  );
  PUA_TimestampsToReturn = ^UA_TimestampsToReturn;
  {$IF sizeof(UA_TimestampsToReturn) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_TIMESTAMPSTORETURN = 121;

(**
 * ReadValueId
 * ^^^^^^^^^^^
 *)
type
  UA_ReadValueId = record
    nodeId: UA_NodeId;
    attributeId: UA_UInt32;
    indexRange: UA_String;
    dataEncoding: UA_QualifiedName;
  end;
  PUA_ReadValueId = ^UA_ReadValueId;

const
  UA_TYPES_READVALUEID = 122;

(**
 * ReadRequest
 * ^^^^^^^^^^^
 *)
type
  UA_ReadRequest = record
    requestHeader: UA_RequestHeader;
    maxAge: UA_Double;
    timestampsToReturn: UA_TimestampsToReturn;
    nodesToReadSize: Size_T;
    nodesToRead: PUA_ReadValueId;
  end;
  PUA_ReadRequest = ^UA_ReadRequest;

const
  UA_TYPES_READREQUEST = 123;

(**
 * ReadResponse
 * ^^^^^^^^^^^^
 *)
type
  UA_ReadResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_DataValue;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_ReadResponse = ^UA_ReadResponse;

const
  UA_TYPES_READRESPONSE = 124;

(**
 * WriteValue
 * ^^^^^^^^^^
 *)
type
  UA_WriteValue = record
    nodeId: UA_NodeId;
    attributeId: UA_UInt32;
    indexRange: UA_String;
    value: UA_DataValue;
  end;
  PUA_WriteValue = ^UA_WriteValue;

const
  UA_TYPES_WRITEVALUE = 125;

(**
 * WriteRequest
 * ^^^^^^^^^^^^
 *)
type
  UA_WriteRequest = record
    requestHeader: UA_RequestHeader;
    nodesToWriteSize: Size_T;
    nodesToWrite: PUA_WriteValue;
  end;
  PUA_WriteRequest = ^UA_WriteRequest;

const
  UA_TYPES_WRITEREQUEST = 126;

(**
 * WriteResponse
 * ^^^^^^^^^^^^^
 *)
type
  UA_WriteResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_StatusCode;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_WriteResponse = ^UA_WriteResponse;

const
  UA_TYPES_WRITERESPONSE = 127;

(**
 * CallMethodRequest
 * ^^^^^^^^^^^^^^^^^
 *)
type
  UA_CallMethodRequest = record
    objectId: UA_NodeId;
    methodId: UA_NodeId;
    inputArgumentsSize: Size_T;
    inputArguments: PUA_Variant;
  end;
  PUA_CallMethodRequest = ^UA_CallMethodRequest;

const
  UA_TYPES_CALLMETHODREQUEST = 128;

(**
 * CallMethodResult
 * ^^^^^^^^^^^^^^^^
 *)
type
  UA_CallMethodResult = record
    statusCode: UA_StatusCode;
    inputArgumentResultsSize: Size_T;
    inputArgumentResults: PUA_StatusCode;
    inputArgumentDiagnosticInfosSize: Size_T;
    inputArgumentDiagnosticInfos: PUA_DiagnosticInfo;
    outputArgumentsSize: Size_T;
    outputArguments: PUA_Variant;
  end;
  PUA_CallMethodResult = ^UA_CallMethodResult;

const
  UA_TYPES_CALLMETHODRESULT = 129;

(**
 * CallRequest
 * ^^^^^^^^^^^
 *)
type
  UA_CallRequest = record
    requestHeader: UA_RequestHeader;
    methodsToCallSize: Size_T;
    methodsToCall: PUA_CallMethodRequest;
  end;
  PUA_CallRequest = ^UA_CallRequest;

const
  UA_TYPES_CALLREQUEST = 130;

(**
 * CallResponse
 * ^^^^^^^^^^^^
 *)
type
  UA_CallResponse = record
    responseHeader: UA_ResponseHeader;
    resultsSize: Size_T;
    results: PUA_CallMethodResult;
    diagnosticInfosSize: Size_T;
    diagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_CallResponse = ^UA_CallResponse;

const
  UA_TYPES_CALLRESPONSE = 131;

(**
 * MonitoringMode
 * ^^^^^^^^^^^^^^
 *)
type
  UA_MonitoringMode = (
    UA_MONITORINGMODE_DISABLED = 0,
    UA_MONITORINGMODE_SAMPLING = 1,
    UA_MONITORINGMODE_REPORTING = 2,
    __UA_MONITORINGMODE_FORCE32BIT = $7fffffff
  );
  PUA_MonitoringMode = ^UA_MonitoringMode;
  {$IF sizeof(UA_MonitoringMode) <> sizeof(UA_Int32)}
    {$MESSAGE ERROR 'enum_must_be_32bit'}
  {$IFEND}

const
  UA_TYPES_MONITORINGMODE = 132;

(**
 * DataChangeTrigger
 * ^^^^^^^^^^^^^^^^^
 *)
type
  UA_DataChangeTrigger = (
    UA_DATACHANGETRIGGER_STATUS = 0,
    UA_DATACHANGETRIGGER_STATUSVALUE = 1,
    UA_DATACHANGETRIGGER_STATUSVALUETIMESTAMP = 2,
    __UA_DATACHANGETRIGGER_FORCE32BIT = $7fffffff
  );
  PUA_DataChangeTrigger = ^UA_DataChangeTrigger;
  {$IF sizeof(UA_DataChangeTrigger) <> sizeof(UA_Int32)}
    {$MESSAGE ERROR 'enum_must_be_32bit'}
  {$IFEND}

const
  UA_TYPES_DATACHANGETRIGGER = 133;

(**
 * DeadbandType
 * ^^^^^^^^^^^^
 *)
type
  UA_DeadbandType = (
    UA_DEADBANDTYPE_NONE = 0,
    UA_DEADBANDTYPE_ABSOLUTE = 1,
    UA_DEADBANDTYPE_PERCENT = 2,
    __UA_DEADBANDTYPE_FORCE32BIT = $7fffffff
  );
  PUA_DeadbandType = ^UA_DeadbandType;
  {$IF sizeof(UA_DeadbandType) <> sizeof(UA_Int32)}
    {$MESSAGE ERROR 'enum_must_be_32bit'}
  {$IFEND}

const
  UA_TYPES_DEADBANDTYPE = 134;

(**
 * DataChangeFilter
 * ^^^^^^^^^^^^^^^^
 *)
type
  UA_DataChangeFilter = record
    Trigger: UA_DataChangeTrigger;
    DeadbandType: UA_UInt32;
    DeadbandValue: UA_Double;
  end;
  PUA_DataChangeFilter = ^UA_DataChangeFilter;

const
  UA_TYPES_DATACHANGEFILTER = 135;

(**
 * EventFilter
 * ^^^^^^^^^^^
 *)
type
  UA_EventFilter = record
    SelectClausesSize: Size_T;
    SelectClauses: PUA_SimpleAttributeOperand;
    WhereClause: UA_ContentFilter;
  end;
  PUA_EventFilter = ^UA_EventFilter;

const
  UA_TYPES_EVENTFILTER = 136;

(**
 * AggregateConfiguration
 * ^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_AggregateConfiguration = record
    UseServerCapabilitiesDefaults: UA_Boolean;
    TreatUncertainAsBad: UA_Boolean;
    PercentDataBad: UA_Byte;
    PercentDataGood: UA_Byte;
    UseSlopedExtrapolation: UA_Boolean;
  end;
  PUA_AggregateConfiguration = ^UA_AggregateConfiguration;

const
  UA_TYPES_AGGREGATECONFIGURATION = 137;

(**
 * AggregateFilter
 * ^^^^^^^^^^^^^^^
 *)
type
  UA_AggregateFilter = record
    StartTime: UA_DateTime;
    AggregateType: UA_NodeId;
    ProcessingInterval: UA_Double;
    AggregateConfiguration: UA_AggregateConfiguration;
  end;
  PUA_AggregateFilter = ^UA_AggregateFilter;

const
  UA_TYPES_AGGREGATEFILTER = 138;

(**
 * EventFilterResult
 * ^^^^^^^^^^^^^^^^^
 *)
type
  UA_EventFilterResult = record
    SelectClauseResultsSize: Size_T;
    SelectClauseResults: PUA_StatusCode;
    SelectClauseDiagnosticInfosSize: Size_T;
    SelectClauseDiagnosticInfos: PUA_DiagnosticInfo;
    WhereClauseResult: UA_ContentFilterResult;
  end;
  PUA_EventFilterResult = ^UA_EventFilterResult;

const
  UA_TYPES_EVENTFILTERRESULT = 139;

(**
 * MonitoringParameters
 * ^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_MonitoringParameters = record
    ClientHandle: UA_UInt32;
    SamplingInterval: UA_Double;
    Filter: UA_ExtensionObject;
    QueueSize: UA_UInt32;
    DiscardOldest: UA_Boolean;
  end;
  PUA_MonitoringParameters = ^UA_MonitoringParameters;

const
  UA_TYPES_MONITORINGPARAMETERS = 140;

(**
 * MonitoredItemCreateRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_MonitoredItemCreateRequest = record
    ItemToMonitor: UA_ReadValueId;
    MonitoringMode: UA_MonitoringMode;
    RequestedParameters: UA_MonitoringParameters;
  end;
  PUA_MonitoredItemCreateRequest = ^UA_MonitoredItemCreateRequest;

const
  UA_TYPES_MONITOREDITEMCREATEREQUEST = 141;

(**
 * MonitoredItemCreateResult
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_MonitoredItemCreateResult = record
    StatusCode: UA_StatusCode;
    MonitoredItemId: UA_UInt32;
    RevisedSamplingInterval: UA_Double;
    RevisedQueueSize: UA_UInt32;
    FilterResult: UA_ExtensionObject;
  end;
  PUA_MonitoredItemCreateResult = ^UA_MonitoredItemCreateResult;

const
  UA_TYPES_MONITOREDITEMCREATERESULT = 142;

(**
 * CreateMonitoredItemsRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_CreateMonitoredItemsRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionId: UA_UInt32;
    TimestampsToReturn: UA_TimestampsToReturn;
    ItemsToCreateSize: Size_T;
    ItemsToCreate: PUA_MonitoredItemCreateRequest;
  end;
  PUA_CreateMonitoredItemsRequest = ^UA_CreateMonitoredItemsRequest;

const
  UA_TYPES_CREATEMONITOREDITEMSREQUEST = 143;

(**
 * CreateMonitoredItemsResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_CreateMonitoredItemsResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_MonitoredItemCreateResult;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_CreateMonitoredItemsResponse = ^UA_CreateMonitoredItemsResponse;

const
  UA_TYPES_CREATEMONITOREDITEMSRESPONSE = 144;

(**
 * MonitoredItemModifyRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_MonitoredItemModifyRequest = record
    MonitoredItemId: UA_UInt32;
    RequestedParameters: UA_MonitoringParameters;
  end;
  PUA_MonitoredItemModifyRequest = ^UA_MonitoredItemModifyRequest;

const
  UA_TYPES_MONITOREDITEMMODIFYREQUEST = 145;

(**
 * MonitoredItemModifyResult
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_MonitoredItemModifyResult = record
    StatusCode: UA_StatusCode;
    RevisedSamplingInterval: UA_Double;
    RevisedQueueSize: UA_UInt32;
    FilterResult: UA_ExtensionObject;
  end;
  PUA_MonitoredItemModifyResult = ^UA_MonitoredItemModifyResult;

const
  UA_TYPES_MONITOREDITEMMODIFYRESULT = 146;

(**
 * ModifyMonitoredItemsRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ModifyMonitoredItemsRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionId: UA_UInt32;
    TimestampsToReturn: UA_TimestampsToReturn;
    ItemsToModifySize: Size_T;
    ItemsToModify: PUA_MonitoredItemModifyRequest;
  end;
  PUA_ModifyMonitoredItemsRequest = ^UA_ModifyMonitoredItemsRequest;

const
  UA_TYPES_MODIFYMONITOREDITEMSREQUEST = 147;

(**
 * ModifyMonitoredItemsResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ModifyMonitoredItemsResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_MonitoredItemModifyResult;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_ModifyMonitoredItemsResponse = ^UA_ModifyMonitoredItemsResponse;

const
  UA_TYPES_MODIFYMONITOREDITEMSRESPONSE = 148;

(**
 * SetMonitoringModeRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SetMonitoringModeRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionId: UA_UInt32;
    MonitoringMode: UA_MonitoringMode;
    MonitoredItemIdsSize: Size_T;
    MonitoredItemIds: PUA_UInt32;
  end;
  PUA_SetMonitoringModeRequest = ^UA_SetMonitoringModeRequest;

const
  UA_TYPES_SETMONITORINGMODEREQUEST = 149;

(**
 * SetMonitoringModeResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SetMonitoringModeResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_StatusCode;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_SetMonitoringModeResponse = ^UA_SetMonitoringModeResponse;

const
  UA_TYPES_SETMONITORINGMODERESPONSE = 150;

(**
 * SetTriggeringRequest
 * ^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SetTriggeringRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionId: UA_UInt32;
    TriggeringItemId: UA_UInt32;
    LinksToAddSize: Size_T;
    LinksToAdd: PUA_UInt32;
    LinksToRemoveSize: Size_T;
    LinksToRemove: PUA_UInt32;
  end;
  PUA_SetTriggeringRequest = ^UA_SetTriggeringRequest;

const
  UA_TYPES_SETTRIGGERINGREQUEST = 151;

(**
 * SetTriggeringResponse
 * ^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SetTriggeringResponse = record
    ResponseHeader: UA_ResponseHeader;
    AddResultsSize: Size_T;
    AddResults: PUA_StatusCode;
    AddDiagnosticInfosSize: Size_T;
    AddDiagnosticInfos: PUA_DiagnosticInfo;
    RemoveResultsSize: Size_T;
    RemoveResults: PUA_StatusCode;
    RemoveDiagnosticInfosSize: Size_T;
    RemoveDiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_SetTriggeringResponse = ^UA_SetTriggeringResponse;

const
  UA_TYPES_SETTRIGGERINGRESPONSE = 152;

(**
 * DeleteMonitoredItemsRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_DeleteMonitoredItemsRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionId: UA_UInt32;
    MonitoredItemIdsSize: Size_T;
    MonitoredItemIds: PUA_UInt32;
  end;
  PUA_DeleteMonitoredItemsRequest = ^UA_DeleteMonitoredItemsRequest;

const
  UA_TYPES_DELETEMONITOREDITEMSREQUEST = 153;

(**
 * DeleteMonitoredItemsResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_DeleteMonitoredItemsResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_StatusCode;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_DeleteMonitoredItemsResponse = ^UA_DeleteMonitoredItemsResponse;

const
  UA_TYPES_DELETEMONITOREDITEMSRESPONSE = 154;

(**
 * CreateSubscriptionRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_CreateSubscriptionRequest = record
    RequestHeader: UA_RequestHeader;
    RequestedPublishingInterval: UA_Double;
    RequestedLifetimeCount: UA_UInt32;
    RequestedMaxKeepAliveCount: UA_UInt32;
    MaxNotificationsPerPublish: UA_UInt32;
    PublishingEnabled: UA_Boolean;
    Priority: UA_Byte;
  end;
  PUA_CreateSubscriptionRequest = ^UA_CreateSubscriptionRequest;

const
  UA_TYPES_CREATESUBSCRIPTIONREQUEST = 155;

(**
 * CreateSubscriptionResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_CreateSubscriptionResponse = record
    ResponseHeader: UA_ResponseHeader;
    SubscriptionId: UA_UInt32;
    RevisedPublishingInterval: UA_Double;
    RevisedLifetimeCount: UA_UInt32;
    RevisedMaxKeepAliveCount: UA_UInt32;
  end;
  PUA_CreateSubscriptionResponse = ^UA_CreateSubscriptionResponse;

const
  UA_TYPES_CREATESUBSCRIPTIONRESPONSE = 156;

(**
 * ModifySubscriptionRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ModifySubscriptionRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionId: UA_UInt32;
    RequestedPublishingInterval: UA_Double;
    RequestedLifetimeCount: UA_UInt32;
    RequestedMaxKeepAliveCount: UA_UInt32;
    MaxNotificationsPerPublish: UA_UInt32;
    Priority: UA_Byte;
  end;
  PUA_ModifySubscriptionRequest = ^UA_ModifySubscriptionRequest;

const
  UA_TYPES_MODIFYSUBSCRIPTIONREQUEST = 157;

(**
 * ModifySubscriptionResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ModifySubscriptionResponse = record
    ResponseHeader: UA_ResponseHeader;
    RevisedPublishingInterval: UA_Double;
    RevisedLifetimeCount: UA_UInt32;
    RevisedMaxKeepAliveCount: UA_UInt32;
  end;
  PUA_ModifySubscriptionResponse = ^UA_ModifySubscriptionResponse;

const
  UA_TYPES_MODIFYSUBSCRIPTIONRESPONSE = 158;

(**
 * SetPublishingModeRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SetPublishingModeRequest = record
    RequestHeader: UA_RequestHeader;
    PublishingEnabled: UA_Boolean;
    SubscriptionIdsSize: Size_T;
    SubscriptionIds: PUA_UInt32;
  end;
  PUA_SetPublishingModeRequest = ^UA_SetPublishingModeRequest;

const
  UA_TYPES_SETPUBLISHINGMODEREQUEST = 159;

(**
 * SetPublishingModeResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SetPublishingModeResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_StatusCode;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_SetPublishingModeResponse = ^UA_SetPublishingModeResponse;

const
  UA_TYPES_SETPUBLISHINGMODERESPONSE = 160;

(**
 * NotificationMessage
 * ^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_NotificationMessage = record
    SequenceNumber: UA_UInt32;
    PublishTime: UA_DateTime;
    NotificationDataSize: Size_T;
    NotificationData: PUA_ExtensionObject;
  end;
  PUA_NotificationMessage = ^UA_NotificationMessage;

const
  UA_TYPES_NOTIFICATIONMESSAGE = 161;

(**
 * MonitoredItemNotification
 * ^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_MonitoredItemNotification = record
    ClientHandle: UA_UInt32;
    Value: UA_DataValue;
  end;
  PUA_MonitoredItemNotification = ^UA_MonitoredItemNotification;

const
  UA_TYPES_MONITOREDITEMNOTIFICATION = 162;

(**
 * EventFieldList
 * ^^^^^^^^^^^^^^
 *)
type
  UA_EventFieldList = record
    ClientHandle: UA_UInt32;
    EventFieldsSize: Size_T;
    EventFields: PUA_Variant;
  end;
  PUA_EventFieldList = ^UA_EventFieldList;

const
  UA_TYPES_EVENTFIELDLIST = 163;

(**
 * StatusChangeNotification
 * ^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_StatusChangeNotification = record
    Status: UA_StatusCode;
    DiagnosticInfo: UA_DiagnosticInfo;
  end;
  PUA_StatusChangeNotification = ^UA_StatusChangeNotification;

const
  UA_TYPES_STATUSCHANGENOTIFICATION = 164;

(**
 * SubscriptionAcknowledgement
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_SubscriptionAcknowledgement = record
    SubscriptionId: UA_UInt32;
    SequenceNumber: UA_UInt32;
  end;
  PUA_SubscriptionAcknowledgement = ^UA_SubscriptionAcknowledgement;

const
  UA_TYPES_SUBSCRIPTIONACKNOWLEDGEMENT = 165;

(**
 * PublishRequest
 * ^^^^^^^^^^^^^^
 *)
type
  UA_PublishRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionAcknowledgementsSize: Size_T;
    SubscriptionAcknowledgements: PUA_SubscriptionAcknowledgement;
  end;
  PUA_PublishRequest = ^UA_PublishRequest;

const
  UA_TYPES_PUBLISHREQUEST = 166;

(**
 * PublishResponse
 * ^^^^^^^^^^^^^^^
 *)
type
  UA_PublishResponse = record
    ResponseHeader: UA_ResponseHeader;
    SubscriptionId: UA_UInt32;
    AvailableSequenceNumbersSize: Size_T;
    AvailableSequenceNumbers: PUA_UInt32;
    MoreNotifications: UA_Boolean;
    NotificationMessage: UA_NotificationMessage;
    ResultsSize: Size_T;
    Results: PUA_StatusCode;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_PublishResponse = ^UA_PublishResponse;

const
  UA_TYPES_PUBLISHRESPONSE = 167;

(**
 * RepublishRequest
 * ^^^^^^^^^^^^^^^^
 *)
type
  UA_RepublishRequest = record
    RequestHeader: UA_RequestHeader;
    RubscriptionId: UA_UInt32;
    RetransmitSequenceNumber: UA_UInt32;
  end;
  PUA_RepublishRequest = ^UA_RepublishRequest;

const
  UA_TYPES_REPUBLISHREQUEST = 168;

(**
 * RepublishResponse
 * ^^^^^^^^^^^^^^^^^
 *)
type
  UA_RepublishResponse = record
    ResponseHeader: UA_ResponseHeader;
    NotificationMessage: UA_NotificationMessage;
  end;
  PUA_RepublishResponse = ^UA_RepublishResponse;

const
  UA_TYPES_REPUBLISHRESPONSE = 169;

(**
 * TransferResult
 * ^^^^^^^^^^^^^^
 *)
type
  UA_TransferResult = record
    StatusCode: UA_StatusCode;
    AvailableSequenceNumbersSize: Size_T;
    AvailableSequenceNumbers: PUA_UInt32;
  end;
  PUA_TransferResult = ^UA_TransferResult;

const
  UA_TYPES_TRANSFERRESULT = 170;

(**
 * TransferSubscriptionsRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_TransferSubscriptionsRequest = record
    RequestHeader: UA_RequestHeader;
    SubscriptionIdsSize: Size_T;
    SubscriptionIds: PUA_UInt32;
    SendInitialValues: UA_Boolean;
  end;
  PUA_TransferSubscriptionsRequest = ^UA_TransferSubscriptionsRequest;

const
  UA_TYPES_TRANSFERSUBSCRIPTIONSREQUEST = 171;

(**
 * TransferSubscriptionsResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_TransferSubscriptionsResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_TransferResult;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_TransferSubscriptionsResponse = ^UA_TransferSubscriptionsResponse;

const
  UA_TYPES_TRANSFERSUBSCRIPTIONSRESPONSE = 172;

(**
 * DeleteSubscriptionsRequest
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_DeleteSubscriptionsRequest = record
    requestHeader: UA_RequestHeader;
    subscriptionIdsSize: Size_T;
    subscriptionIds: PUA_UInt32;
  end;
  PUA_DeleteSubscriptionsRequest = ^UA_DeleteSubscriptionsRequest;

const
  UA_TYPES_DELETESUBSCRIPTIONSREQUEST = 173;

(**
 * DeleteSubscriptionsResponse
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_DeleteSubscriptionsResponse = record
    ResponseHeader: UA_ResponseHeader;
    ResultsSize: Size_T;
    Results: PUA_StatusCode;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_DeleteSubscriptionsResponse = ^UA_DeleteSubscriptionsResponse;

const
  UA_TYPES_DELETESUBSCRIPTIONSRESPONSE = 174;

(**
 * BuildInfo
 * ^^^^^^^^^
 *)
type
  UA_BuildInfo = record
    ProductUri: UA_String;
    ManufacturerName: UA_String;
    ProductName: UA_String;
    SoftwareVersion: UA_String;
    BuildNumber: UA_String;
    BuildDate: UA_DateTime;
  end;
  PUA_BuildInfo = ^UA_BuildInfo;

const
  UA_TYPES_BUILDINFO = 175;

(**
 * RedundancySupport
 * ^^^^^^^^^^^^^^^^^
 *)
type
  UA_RedundancySupport = (
    UA_REDUNDANCYSUPPORT_NONE = 0,
    UA_REDUNDANCYSUPPORT_COLD = 1,
    UA_REDUNDANCYSUPPORT_WARM = 2,
    UA_REDUNDANCYSUPPORT_HOT = 3,
    UA_REDUNDANCYSUPPORT_TRANSPARENT = 4,
    UA_REDUNDANCYSUPPORT_HOTANDMIRRORED = 5,
    __UA_REDUNDANCYSUPPORT_FORCE32BIT = $7fffffff
  );
  PUA_RedundancySupport = ^UA_RedundancySupport;
  {$IF sizeof(UA_RedundancySupport) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_REDUNDANCYSUPPORT = 176;

(**
 * ServerState
 * ^^^^^^^^^^^
 *)
type
  UA_ServerState = (
    UA_SERVERSTATE_RUNNING = 0,
    UA_SERVERSTATE_FAILED = 1,
    UA_SERVERSTATE_NOCONFIGURATION = 2,
    UA_SERVERSTATE_SUSPENDED = 3,
    UA_SERVERSTATE_SHUTDOWN = 4,
    UA_SERVERSTATE_TEST = 5,
    UA_SERVERSTATE_COMMUNICATIONFAULT = 6,
    UA_SERVERSTATE_UNKNOWN = 7,
    __UA_SERVERSTATE_FORCE32BIT = $7fffffff
  );
  PUA_ServerState = ^UA_ServerState;
  {$IF sizeof(UA_ServerState) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_SERVERSTATE = 177;

(**
 * ServerDiagnosticsSummaryDataType
 * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ServerDiagnosticsSummaryDataType = record
    ServerViewCount: UA_UInt32;
    CurrentSessionCount: UA_UInt32;
    CumulatedSessionCount: UA_UInt32;
    SecurityRejectedSessionCount: UA_UInt32;
    RejectedSessionCount: UA_UInt32;
    SessionTimeoutCount: UA_UInt32;
    SessionAbortCount: UA_UInt32;
    CurrentSubscriptionCount: UA_UInt32;
    CumulatedSubscriptionCount: UA_UInt32;
    PublishingIntervalCount: UA_UInt32;
    SecurityRejectedRequestsCount: UA_UInt32;
    RejectedRequestsCount: UA_UInt32;
  end;
  PUA_ServerDiagnosticsSummaryDataType = ^UA_ServerDiagnosticsSummaryDataType;

const
  UA_TYPES_SERVERDIAGNOSTICSSUMMARYDATATYPE = 178;

(**
 * ServerStatusDataType
 * ^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_ServerStatusDataType = record
    StartTime: UA_DateTime;
    CurrentTime: UA_DateTime;
    State: UA_ServerState;
    BuildInfo: UA_BuildInfo;
    SecondsTillShutdown: UA_UInt32;
    ShutdownReason: UA_LocalizedText;
  end;
  PUA_ServerStatusDataType = ^UA_ServerStatusDataType;

const
  UA_TYPES_SERVERSTATUSDATATYPE = 179;

(**
 * Range
 * ^^^^^
 *)
type
  UA_Range = record
    Low: UA_Double;
    High: UA_Double;
  end;
  PUA_Range = ^UA_Range;

const
  UA_TYPES_RANGE = 180;

(**
 * EUInformation
 * ^^^^^^^^^^^^^
 *)
type
  UA_EUInformation = record
    NamespaceUri: UA_String;
    UnitId: UA_Int32;
    DisplayName: UA_LocalizedText;
    Description: UA_LocalizedText;
  end;
  PUA_EUInformation = ^UA_EUInformation;

const
  UA_TYPES_EUINFORMATION = 181;

(**
 * AxisScaleEnumeration
 * ^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_AxisScaleEnumeration = (
    UA_AXISSCALEENUMERATION_LINEAR = 0,
    UA_AXISSCALEENUMERATION_LOG = 1,
    UA_AXISSCALEENUMERATION_LN = 2,
    __UA_AXISSCALEENUMERATION_FORCE32BIT = $7fffffff
  );
  PUA_AxisScaleEnumeration = ^UA_AxisScaleEnumeration;
  {$IF sizeof(UA_AxisScaleEnumeration) <> sizeof(UA_Int32)}{$MESSAGE ERROR 'enum_must_be_32bit'}{$IFEND}

const
  UA_TYPES_AXISSCALEENUMERATION = 182;

(**
 * ComplexNumberType
 * ^^^^^^^^^^^^^^^^^
 *)
type
  UA_ComplexNumberType = record
    Real: UA_Float;
    Imaginary: UA_Float;
  end;
  PUA_ComplexNumberType = ^UA_ComplexNumberType;

const
  UA_TYPES_COMPLEXNUMBERTYPE = 183;

(**
 * DoubleComplexNumberType
 * ^^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_DoubleComplexNumberType = record
    Real: UA_Double;
    Imaginary: UA_Double;
  end;
  PUA_DoubleComplexNumberType = ^UA_DoubleComplexNumberType;

const
  UA_TYPES_DOUBLECOMPLEXNUMBERTYPE = 184;

(**
 * AxisInformation
 * ^^^^^^^^^^^^^^^
 *)
type
  UA_AxisInformation = record
    EngineeringUnits: UA_EUInformation;
    EURange: UA_Range;
    Title: UA_LocalizedText;
    AxisScaleType: UA_AxisScaleEnumeration;
    AxisStepsSize: Size_T;
    AxisSteps: PUA_Double;
  end;
  PUA_AxisInformation = ^UA_AxisInformation;

const
  UA_TYPES_AXISINFORMATION = 185;

(**
 * XVType
 * ^^^^^^
 *)
type
  UA_XVType = record
    X: UA_Double;
    Value: UA_Float;
  end;
  PUA_XVType = ^UA_XVType;

const
  UA_TYPES_XVTYPE = 186;

(**
 * EnumDefinition
 * ^^^^^^^^^^^^^^
 *)
type
  UA_EnumDefinition = record
    FieldsSize: Size_T;
    Fields: PUA_EnumField;
  end;
  PUA_EnumDefinition = ^UA_EnumDefinition;

const
  UA_TYPES_ENUMDEFINITION = 187;

(**
 * DataChangeNotification
 * ^^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_DataChangeNotification = record
    MonitoredItemsSize: Size_T;
    MonitoredItems: PUA_MonitoredItemNotification;
    DiagnosticInfosSize: Size_T;
    DiagnosticInfos: PUA_DiagnosticInfo;
  end;
  PUA_DataChangeNotification = ^UA_DataChangeNotification;

const
  UA_TYPES_DATACHANGENOTIFICATION = 188;

(**
 * EventNotificationList
 * ^^^^^^^^^^^^^^^^^^^^^
 *)
type
  UA_EventNotificationList = record
    EventsSize: Size_T;
    Events: PUA_EventFieldList;
  end;
  PUA_EventNotificationList = ^UA_EventNotificationList;

const
  UA_TYPES_EVENTNOTIFICATIONLIST = 189;
