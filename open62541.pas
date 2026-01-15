(*
 * open62541 is licensed under the Mozilla Public License v2.0 (MPLv2).
 * This allows the open62541 library to be combined and distributed with any proprietary software.
 * Only changes to the open62541 library itself need to be licensed under the MPLv2 when copied and distributed.
 * The plugins, as well as the server and client examples are in the public domain (CC0 license).
 * They can be reused under any license and changes do not have to be published.
 *
 * Version 1.2-rc2 released on 23 Dec 2020
 *
 * BEWARE: between version 1.1 and version 1.2 many structures
 * and ids have changed so the two versions are not binary compatible
 * (i.e you cannot use these headers with version 1.1)
 *
 * Author: Lacak <lacak At Sourceforge>
 *
 * Contributors: 
 *   Luca Olivetti <luca@ventoso.org>
 *)
unit open62541;

(*
 * Example:
 *   var
 *     Client: PUA_Client;
 *     Config: PUA_ClientConfig;
 *     NodeId: UA_NodeId;
 *     Value: UA_Variant;
 *   begin
 *     Client := UA_Client_new();
 *     Config := UA_Client_getConfig(Client);
 *     UA_ClientConfig_setDefault(Config);
 *     if UA_Client_connect(Client, 'opc.tcp://localhost...') = UA_STATUSCODE_GOOD then 
       begin
 *       NodeId := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME);
 *       UA_Variant_init(value);
 *       if (UA_Client_readValueAttribute(Client, NodeId, Value) = UA_STATUSCODE_GOOD) and
 *          (UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DATETIME])) then
         begin
 *          ...
 *       end;
 *       UA_Variant_clear(Value);
 *     end;
 *     UA_Client_delete(Client);
 *   end;
 *)

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

// Use open62541 v1.3
{ $DEFINE UA_VER1_3}
{$IFNDEF UA_VER1_3}
  {$DEFINE UA_VER1_2}
{$ENDIF}
interface

{ ---------------- }
{ --- config.h --- }
{ ---------------- }
{$DEFINE UA_ENABLE_METHODCALLS}
{$DEFINE UA_ENABLE_SUBSCRIPTIONS}
{$DEFINE UA_ENABLE_STATUSCODE_DESCRIPTIONS}
{$DEFINE UA_ENABLE_TYPEDESCRIPTION}
{ $DEFINE UA_ENABLE_ENCRYPTION} // disabled in pre-compiled "libopen62541" library (required for SIGN and SIGN&ENCRYPT)

const
{$IFDEF UNIX}
  libopen62541 = 'libopen62541.so';
{$ELSE}
  libopen62541 = 'libopen62541.dll'; // GCC libgcc_s_sjlj-1.dll and libwinpthread-1.dll are also required
                                     //  they can be downloaded from packages at http://win-builds.org/1.5.0/packages/windows_32/
{$ENDIF}

{$IFDEF UA_VER1_3}
  UA_VER = 1.3;
{$ELSE}
  UA_VER = 1.2;
{$ENDIF}

type
  {$IFNDEF FPC}
  // Delphi XE compatibility
  DWord = LongWord; // FixedUInt 32-bit
  Size_T = NativeUInt; // DWord on 32-bit platforms, QWord on 64-bit platforms
  {$ENDIF}
  {$IF NOT DECLARED(Size_T)}
  Size_T = NativeUInt;
  {$IFEND}

  UA_Client = record end;
  PUA_Client = ^UA_Client;

  { --------------- }
  { --- types.h --- }
  { --------------- }

  UA_Boolean = bytebool;
  PUA_Boolean = ^UA_Boolean;

  UA_Byte = Byte;
  PUA_Byte = ^UA_Byte;

  UA_Int16 = Smallint;
  PUA_Int16 = ^UA_Int16;

  UA_UInt16 = Word;
  PUA_UInt16 = ^UA_UInt16;

  UA_Int32 = integer;
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

  (**
   * .. _statuscode:
   *
   * StatusCode
   * ^^^^^^^^^^
   * A numeric identifier for a error or condition that is associated with a value
   * or an operation. See the section :ref:`statuscodes` for the meaning of a
   * specific code. *)
  UA_StatusCode = DWord; // uint32_t
  PUA_StatusCode = ^UA_StatusCode;

  (**
   * String
   * ^^^^^^
   * A sequence of Unicode characters. Strings are just an array of UA_Byte. *)
  UA_String = record
    Length: Size_T; (* The length of the string *)
    Data: PUA_Byte; (* The content (not null-terminated) *)
  end;
  PUA_String = ^UA_String;

  (**
   * .. _datetime:
   *
   * DateTime
   * ^^^^^^^^
   * An instance in time. A DateTime value is encoded as a 64-bit signed integer
   * which represents the number of 100 nanosecond intervals since January 1, 1601
   * (UTC).
   *
   * The methods providing an interface to the system clock are architecture-
   * specific. Usually, they provide a UTC clock that includes leap seconds. The
   * OPC UA standard allows the use of International Atomic Time (TAI) for the
   * DateTime instead. But this is still unusual and not implemented for most
   * SDKs. Currently (2019), UTC and TAI are 37 seconds apart due to leap
   * seconds. *)
  UA_DateTime = Int64;
  PUA_DateTime = ^UA_DateTime;

  UA_DateTimeStruct = record
    NanoSec: UA_UInt16;
    MicroSec: UA_UInt16;
    MilliSec: UA_UInt16;
    Sec: UA_UInt16;
    Min: UA_UInt16;
    Hour: UA_UInt16;
    Day: UA_UInt16;
    Month: UA_UInt16;
    Year: UA_UInt16;
  end;

  (**
   * ByteString
   * ^^^^^^^^^^
   * A sequence of octets. *)
  UA_ByteString = UA_String;
  PUA_ByteString  = ^UA_ByteString;

  (**
   * Guid
   * ^^^^
   * A 16 byte value that can be used as a globally unique identifier. *)
  UA_Guid = record
    Data1: UA_UInt32;
    Data2: UA_UInt16;
    Data3: UA_UInt16 ;
    Data4: array[0..7] of UA_Byte;
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
    (* Log a message. The message string and following varargs are formatted
     * according to the rules of the printf command. Use the convenience macros
     * below that take the minimum log-level defined in ua_config.h into
     * account. *)
    Log: procedure(LogContext: Pointer; Level: UA_LogLevel; Category: UA_LogCategory; Msg: PAnsiChar; Args: {va_list}array of const); cdecl;
    Context: Pointer; (* Logger state *)
    Clear: procedure(Context: Pointer); cdecl; (* Clean up the logger plugin *)
  end;
  PUA_Logger  = ^UA_Logger;

  (**
   * .. _nodeid:
   *
   * NodeId
   * ^^^^^^
   * An identifier for a node in the address space of an OPC UA Server. *)
  UA_NodeIdType = (
    UA_NODEIDTYPE_NUMERIC    = 0, (* In the binary encoding, this can also
                                   * become 1 or 2 (two-byte and four-byte
                                   * encoding of small numeric nodeids) *)
    UA_NODEIDTYPE_STRING     = 3,
    UA_NODEIDTYPE_GUID       = 4,
    UA_NODEIDTYPE_BYTESTRING = 5
  );

  UA_NodeId = record
    NamespaceIndex: UA_UInt16;
    IdentifierType: UA_NodeIdType;
    Identifier: record
      case longint of
        0: (Numeric: UA_UInt32 );
        1: (_String: UA_String );
        2: (GUID: UA_Guid );
        3: (ByteString: UA_ByteString );
      end;
  end;
  PUA_NodeId = ^UA_NodeId;

  (**
   * ExpandedNodeId
   * ^^^^^^^^^^^^^^
   * A NodeId that allows the namespace URI to be specified instead of an index. *)
  UA_ExpandedNodeId = record
    NodeId: UA_NodeId;
    NamespaceUri: UA_String;
    ServerIndex: UA_UInt32;
  end;
  PUA_ExpandedNodeId = ^UA_ExpandedNodeId;

  (**
   * .. _qualifiedname:
   *
   * QualifiedName
   * ^^^^^^^^^^^^^
   * A name qualified by a namespace. *)
  UA_QualifiedName = record
    NamespaceIndex: UA_UInt16;
    Name: UA_String;
  end;
  PUA_QualifiedName = ^UA_QualifiedName;

  (**
   * LocalizedText
   * ^^^^^^^^^^^^^
   * Human readable text with an optional locale identifier. *)
  UA_LocalizedText = record
    Locale: UA_String;
    Text: UA_String;
  end;
  PUA_LocalizedText = ^UA_LocalizedText;

  (**
   * NumericRange
   * ^^^^^^^^^^^^
   * NumericRanges are used to indicate subsets of a (multidimensional) array.
   * They no official data type in the OPC UA standard and are transmitted only
   * with a string encoding, such as "1:2,0:3,5". The colon separates min/max
   * index and the comma separates dimensions. A single value indicates a range
   * with a single element (min==max). *)
  UA_NumericRangeDimension = record
    Min: UA_UInt32;
    Max: UA_UInt32;
  end;

  UA_NumericRange = record
    DimensionsSize: Size_T;
    Dimensions: ^UA_NumericRangeDimension;
  end;
  PUA_NumericRange = ^UA_NumericRange;


  PUA_DataType = ^UA_DataType;

  (**
   * .. _variant:
   *
   * Variant
   * ^^^^^^^
   *
   * Variants may contain values of any type together with a description of the
   * content. See the section on :ref:`generic-types` on how types are described.
   * The standard mandates that variants contain built-in data types only. If the
   * value is not of a builtin type, it is wrapped into an :ref:`extensionobject`.
   * open62541 hides this wrapping transparently in the encoding layer. If the
   * data type is unknown to the receiver, the variant contains the original
   * ExtensionObject in binary or XML encoding.
   *
   * Variants may contain a scalar value or an array. For details on the handling
   * of arrays, see the section on :ref:`array-handling`. Array variants can have
   * an additional dimensionality (matrix, 3-tensor, ...) defined in an array of
   * dimension lengths. The actual values are kept in an array of dimensions one.
   * For users who work with higher-dimensions arrays directly, keep in mind that
   * dimensions of higher rank are serialized first (the highest rank dimension
   * has stride 1 and elements follow each other directly). Usually it is simplest
   * to interact with higher-dimensional arrays via ``UA_NumericRange``
   * descriptions (see :ref:`array-handling`).
   *
   * To differentiate between scalar / array variants, the following definition is
   * used. ``UA_Variant_isScalar`` provides simplified access to these checks.
   *
   * - ``arrayLength == 0 && data == NULL``: undefined array of length -1
   * - ``arrayLength == 0 && data == UA_EMPTY_ARRAY_SENTINEL``: array of length 0
   * - ``arrayLength == 0 && data > UA_EMPTY_ARRAY_SENTINEL``: scalar value
   * - ``arrayLength > 0``: array of the given length
   *
   * Variants can also be *empty*. Then, the Pointer to the type description is
   * ``NULL``. *)
  UA_VariantStorageType = (
    UA_VARIANT_DATA,         (* The data has the same lifecycle as the variant *)
    UA_VARIANT_DATA_NODELETE (* The data is "borrowed" by the variant and shall not be deleted at the end of the variant's lifecycle.*)
  );

  UA_Variant = record
    _Type : PUA_DataType;    (* The data type description *)
    StorageType: UA_VariantStorageType;
    ArrayLength: Size_T;
    Data: Pointer;          (* Points to the scalar or array data *)
    ArrayDimensionsSize: Size_T; (* The number of dimensions *)
    ArrayDimensions: ^UA_UInt32; (* The length of each dimension *)
  end;
  PUA_Variant = ^UA_Variant;

  (**
   * .. _extensionobject:
   *
   * ExtensionObject
   * ^^^^^^^^^^^^^^^
   *
   * ExtensionObjects may contain scalars of any data type. Even those that are
   * unknown to the receiver. See the section on :ref:`generic-types` on how types
   * are described. If the received data type is unknown, the encoded string and
   * target NodeId is stored instead of the decoded value. *)
  UA_ExtensionObjectEncoding = (
    UA_EXTENSIONOBJECT_ENCODED_NOBODY     = 0,
    UA_EXTENSIONOBJECT_ENCODED_BYTESTRING = 1,
    UA_EXTENSIONOBJECT_ENCODED_XML        = 2,
    UA_EXTENSIONOBJECT_DECODED            = 3,
    UA_EXTENSIONOBJECT_DECODED_NODELETE   = 4 (* Don't delete the content
                                                 together with the ExtensionObject *)
  );

  UA_ExtensionObject = record
    encoding: UA_ExtensionObjectEncoding;
    content:  record
                case LongInt of
                  0: ( Encoded: record
                        TypeId: UA_NodeId;   (* The nodeid of the datatype *)
                        Body: UA_ByteString; (* The bytestring of the Encoded data *)
                      end );
                  1: ( Decoded: record
                        _Type: PUA_DataType;
                        Data: Pointer;
                      end );
                end;
  end;
  PUA_ExtensionObject = ^UA_ExtensionObject;

  (**
   * DataValue
   * ^^^^^^^^^
   * A data value with an associated status code and timestamps. *)
  UA_DataValue = record
    Value: UA_Variant;
    SourceTimestamp: UA_DateTime;
    ServerTimestamp: UA_DateTime;
    SourcePicoseconds: UA_UInt16;
    ServerPicoseconds: UA_UInt16;
    Status: UA_StatusCode;
    Flag: UA_Byte;
{    UA_Boolean    hasValue             : 1;
    UA_Boolean    hasStatus            : 1;
    UA_Boolean    hasSourceTimestamp   : 1;
    UA_Boolean    hasServerTimestamp   : 1;
    UA_Boolean    hasSourcePicoseconds : 1;
    UA_Boolean    hasServerPicoseconds : 1;}
  end;
  PUA_DataValue = ^UA_DataValue;

  (**
   * DiagnosticInfo
   * ^^^^^^^^^^^^^^
   * A structure that contains detailed error and diagnostic information
   * associated with a StatusCode. *)
  PUA_DiagnosticInfo = ^UA_DiagnosticInfo;
  UA_DiagnosticInfo = record
    Flag: UA_Boolean;
{   UA_Boolean    hasSymbolicId          : 1;
    UA_Boolean    hasNamespaceUri        : 1;
    UA_Boolean    hasLocalizedText       : 1;
    UA_Boolean    hasLocale              : 1;
    UA_Boolean    hasAdditionalInfo      : 1;
    UA_Boolean    hasInnerStatusCode     : 1;
    UA_Boolean    hasInnerDiagnosticInfo : 1;}
    SymbolicId: UA_Int32;
    NamespaceUri: UA_Int32;
    LocalizedText: UA_Int32;
    Locale: UA_Int32;
    AdditionalInfo: UA_String;
    InnerStatusCode: UA_StatusCode;
    InnerDiagnosticInfo: PUA_DiagnosticInfo;
  end;

  (**
  * .. _generic-types:
  *
  * Generic Type Handling
  * ---------------------
  *
  * All information about a (builtin/structured) data type is stored in a
  * ``UA_DataType``. The array ``UA_TYPES`` contains the description of all
  * standard-defined types. This type description is used for the following
  * generic operations that work on all types:
  *
  * - ``void T_init(T *ptr)``: Initialize the data type. This is synonymous with
  *   zeroing out the memory, i.e. ``memset(ptr, 0, sizeof(T))``.
  * - ``T* T_new()``: Allocate and return the memory for the data type. The
  *   value is already initialized.
  * - ``UA_StatusCode T_copy(const T *src, T *dst)``: Copy the content of the
  *   data type. Returns ``UA_STATUSCODE_GOOD`` or
  *   ``UA_STATUSCODE_BADOUTOFMEMORY``.
  * - ``void T_clear(T *ptr)``: Delete the dynamically allocated content
  *   of the data type and perform a ``T_init`` to reset the type.
  * - ``void T_delete(T *ptr)``: Delete the content of the data type and the
  *   memory for the data type itself.
  *
  * Specializations, such as ``UA_Int32_new()`` are derived from the generic
  * type operations as static inline functions. *)

  {$IFDEF UA_VER1_3}
  UA_DataTypeMember = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    MemberName: PAnsiChar;
    {$ENDIF}
    MemberType: PUA_DataType;
    Padding: 0..63;               (* How much padding is there before this
                                     member element? For arrays this is the
                                     padding before the Size_T length member.
                                     (No padding between Size_T and the
                                     following ptr.) *)
    IsArray : 0..1;
    IsOptional : 0..1;
    Fill : UA_Byte;
    Fill1 : UA_Byte;
    Fill2 : UA_Byte;

    {NamespaceZero: UA_Boolean:1;}  (* The type of the member is defined in
                                      namespace zero. In this implementation,
                                      types from custom namespace may contain
                                      members from the same namespace or
                                      namespace zero only.*)
    {IsArray: UA_Boolean:1;}        (* The member is an array *)
    {IsOptional: UA_Boolean:1;}     (* The member is an optional field *)
  end;
  {$ELSE}
  UA_DataTypeMember = record
   MemberTypeIndex: UA_UInt16;   (* Index of the member in the array of data types *)
   Padding: UA_Byte;             (* How much padding is there before this
                                  member element? For arrays this is the
                                  padding before the Size_T length member.
                                  (No padding between Size_T and the
                                  following ptr.) *)
   Flag: Byte;
   {NamespaceZero: UA_Boolean:1;}  (* The type of the member is defined in
                                    namespace zero. In this implementation,
                                    types from custom namespace may contain
                                    members from the same namespace or
                                    namespace zero only.*)
   //IsArray: UA_Boolean:1;        (* The member is an array *)
   //IsOptional: UA_Boolean:1;     (* The member is an optional field *)
   {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
   MemberName: PAnsiChar;
   {$ENDIF}
  end;
  {$ENDIF}

   (* The DataType "kind" is an internal type classification. It is used to
    * dispatch handling to the correct routines. *)
   UA_DataTypeKind = (
     UA_DATATYPEKIND_BOOLEAN = 0,
     UA_DATATYPEKIND_SBYTE = 1,
     UA_DATATYPEKIND_BYTE = 2,
     UA_DATATYPEKIND_INT16 = 3,
     UA_DATATYPEKIND_UINT16 = 4,
     UA_DATATYPEKIND_INT32 = 5,
     UA_DATATYPEKIND_UINT32 = 6,
     UA_DATATYPEKIND_INT64 = 7,
     UA_DATATYPEKIND_UINT64 = 8,
     UA_DATATYPEKIND_FLOAT = 9,
     UA_DATATYPEKIND_DOUBLE = 10,
     UA_DATATYPEKIND_STRING = 11,
     UA_DATATYPEKIND_DATETIME = 12,
     UA_DATATYPEKIND_GUID = 13,
     UA_DATATYPEKIND_BYTESTRING = 14,
     UA_DATATYPEKIND_XMLELEMENT = 15,
     UA_DATATYPEKIND_NODEID = 16,
     UA_DATATYPEKIND_EXPANDEDNODEID = 17,
     UA_DATATYPEKIND_STATUSCODE = 18,
     UA_DATATYPEKIND_QUALIFIEDNAME = 19,
     UA_DATATYPEKIND_LOCALIZEDTEXT = 20,
     UA_DATATYPEKIND_EXTENSIONOBJECT = 21,
     UA_DATATYPEKIND_DATAVALUE = 22,
     UA_DATATYPEKIND_VARIANT = 23,
     UA_DATATYPEKIND_DIAGNOSTICINFO = 24,
     UA_DATATYPEKIND_DECIMAL = 25,
     UA_DATATYPEKIND_ENUM = 26,
     UA_DATATYPEKIND_STRUCTURE = 27,
     UA_DATATYPEKIND_OPTSTRUCT = 28,      (* struct with optional fields *)
     UA_DATATYPEKIND_UNION = 29,
     UA_DATATYPEKIND_BITFIELDCLUSTER = 30 (* bitfields + padding *)
   );

   {$IFDEF UA_VER1_3}
   UA_DataType = bitpacked record
     {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
     TypeName: PAnsiChar;
     {$ENDIF}
     TypeId: UA_NodeId;               (* The nodeid of the type *)
     BinaryEncodingId: UA_NodeId;     (* NodeId of datatype when encoded as binary *)
     //xmlEncodingId: UA_NodeId;      (* NodeId of datatype when encoded as XML *)
     MemSize: UA_UInt16;              (* Size of the struct in memory *)
     TypeKind: 0..63;                 (* Dispatch index for the handling routines *)
     PointerFree: 0..1;               (* The type (and its members) contains no
                                      * pointers that need to be freed *)
     Overlayable: 0..1;               (* The type has the identical memory layout
                                      * in memory and on the binary stream. *)
     MembersSize: UA_Byte;            (* How many members does the type have? *)
     Members: ^UA_DataTypeMember;
   end;
   {$ELSE}
//   UA_DataType = bitpacked record
   UA_DataType = record
     TypeId: UA_NodeId;               (* The nodeid of the type *)
     BinaryEncodingId: UA_NodeId;     (* NodeId of datatype when encoded as binary *)
     MemSize: UA_UInt16;              (* Size of the struct in memory *)
     TypeIndex: UA_UInt16;            (* Index of the type in the datatypetable *)
//     TypeKind : 0..63;                (* Dispatch index for the handling routines *)
//     PointerFree : 0..1;              (* The type (and its members) contains no
     Flags: UA_Int32;
     {UA_UInt32 typeKind        : 6;} (* Dispatch index for the handling routines *)
     {UA_UInt32 pointerFree     : 1;} (* The type (and its members) contains no
                                       * pointers that need to be freed *)
//       Overlayable : 0..1;              (* The type has the identical memory layout
       {UA_UInt32 overlayable     : 1;} (* The type has the identical memory layout
                                         * in memory and on the binary stream. *)
//       MembersSize : UA_Byte;           (* How many members does the type have? *)
     {UA_UInt32 membersSize     : 8;} (* How many members does the type have? *)
     //UA_UInt16  xmlEncodingId;      (* NodeId of datatype when encoded as XML *)
     Members: ^UA_DataTypeMember;
     {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
     TypeName: PAnsiChar;
     {$ENDIF}
   end;
   {$ENDIF}

  (* Datatype arrays with custom type definitions can be added in a linked list to
   * the client or server configuration. Datatype members can point to types in
   * the same array via the ``memberTypeIndex``. If ``namespaceZero`` is set to
   * true, the member datatype is looked up in the array of builtin datatypes
   * instead. *)
  PUA_DataTypeArray = ^UA_DataTypeArray;
  UA_DataTypeArray = record
    Next: PUA_DataTypeArray;
    TypesSize: Size_T;
    Types: ^UA_DataType;
  end;


  { ------------------------- }
  { --- types_generated.h --- }
  { ------------------------- }
{$IFDEF UA_VER1_3}
  {$I types_generated_1_3.inc}
{$ELSE}
  {$I types_generated.inc}
{$ENDIF}

  { ---------------- }
  { --- common.h --- }
  { ---------------- }

  type

  (**
   * Standard-Defined Constants
   * ==========================
   * This section contains numerical and string constants that are defined in the
   * OPC UA standard.
   *
   * .. _attribute-id:
   *
   * Attribute Id
   * ------------
   * Every node in an OPC UA information model contains attributes depending on
   * the node type. Possible attributes are as follows: *)

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

  (**
   * Rule Handling
   * -------------
   *
   * The RuleHanding settings define how error cases that result from rules in the
   * OPC UA specification shall be handled. The rule handling can be softened,
   * e.g. to workaround misbehaving implementations or to mitigate the impact of
   * additional rules that are introduced in later versions of the OPC UA
   * specification. *)
  UA_RuleHandling = (
    UA_RULEHANDLING_DEFAULT = 0,
    UA_RULEHANDLING_ABORT,  (* Abort the operation and return an error code *)
    UA_RULEHANDLING_WARN,   (* Print a message in the logs and continue *)
    UA_RULEHANDLING_ACCEPT  (* Continue and disregard the broken rule *)
  );


  (**
   * Connection State
   * ---------------- *)
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

  (**
   * Statistic counters
   * ------------------
   *
   * The stack manage statistic counter for the following layers:
   *
   * - Network
   * - Secure channel
   * - Session
   *
   * The session layer counters are matching the counters of the
   * ServerDiagnosticsSummaryDataType that are defined in the OPC UA Part 5
   * specification. Counter of the other layers are not specified by OPC UA but
   * are harmonized with the session layer counters if possible. *)

  UA_NetworkStatistics = record
    CurrentConnectionCount: Size_T;
    CumulatedConnectionCount: Size_T;
    RejectedConnectionCount: Size_T;
    ConnectionTimeoutCount: Size_T;
    ConnectionAbortCount: Size_T;
  end;
  PUA_NetworkStatistics = ^UA_NetworkStatistics;

  UA_SecureChannelStatistics = record
    CurrentChannelCount: Size_T;
    CumulatedChannelCount: Size_T;
    RejectedChannelCount: Size_T;
    ChannelTimeoutCount: Size_T; (* only used by servers *)
    ChannelAbortCount: Size_T;
    ChannelPurgeCount: Size_T;   (* only used by servers *)
  end;
  PUA_SecureChannelStatistics = ^UA_SecureChannelStatistics;

  UA_SessionStatistics = record
    CurrentSessionCount: Size_T;
    CumulatedSessionCount: Size_T;
    SecurityRejectedSessionCount: Size_T; (* only used by servers *)
    RejectedSessionCount: Size_T;
    SessionTimeoutCount: Size_T;          (* only used by servers *)
    SessionAbortCount: Size_T;            (* only used by servers *)
  end;
  PUA_SessionStatistics = ^UA_SessionStatistics;

  { ----------------- }
  { --- network.h --- }
  { ----------------- }

  (**
   * .. _networking:
   *
   * Networking Plugin API
   * =====================
   *
   * Connection
   * ----------
   * Client-server connections are represented by a `UA_Connection`. The
   * connection is stateful and stores partially received messages, and so on. In
   * addition, the connection contains function pointers to the underlying
   * networking implementation. An example for this is the `send` function. So the
   * connection encapsulates all the required networking functionality. This lets
   * users on embedded (or otherwise exotic) systems implement their own
   * networking plugins with a clear interface to the main open62541 library. *)

  UA_ConnectionConfig = record
    protocolVersion: UA_UInt32;
    recvBufferSize: UA_UInt32;
    sendBufferSize: UA_UInt32;
    localMaxMessageSize: UA_UInt32;   (*  (0 = unbounded) *)
    remoteMaxMessageSize: UA_UInt32;  (*  (0 = unbounded) *)
    localMaxChunkCount: UA_UInt32;    (*  (0 = unbounded) *)
    remoteMaxChunkCount: UA_UInt32;   (*  (0 = unbounded) *)
  end;

 UA_ConnectionState = (UA_CONNECTION_CLOSED, UA_CONNECTION_OPENING, UA_CONNECTION_ESTABLISHED);

 UA_SecureChannel = record {undefined structure} end;

 UA_SOCKET = Integer;

 PUA_Connection  = ^UA_Connection;
 UA_Connection = record
      state : UA_ConnectionState;
      config : UA_ConnectionConfig;
      channel : ^UA_SecureChannel;
      sockfd : UA_SOCKET;
      openingDate : UA_DateTime;
      handle : Pointer;
      incompleteChunk : UA_ByteString;
      connectCallbackID : UA_UInt64;
      getSendBuffer : function (connection:PUA_Connection; length:Size_T; buf:PUA_ByteString):UA_StatusCode;cdecl;
      releaseSendBuffer : procedure (connection:PUA_Connection; buf:PUA_ByteString);cdecl;
      send : function (connection:PUA_Connection; buf:PUA_ByteString):UA_StatusCode;cdecl;
      recv : function (connection:PUA_Connection; response:PUA_ByteString; timeout:UA_UInt32):UA_StatusCode;cdecl;
      releaseRecvBuffer : procedure (connection:PUA_Connection; buf:PUA_ByteString);cdecl;
      close : procedure (connection:PUA_Connection);cdecl;
      free : procedure (connection:PUA_Connection);cdecl;
    end;

 UA_ConnectClientConnection = function (config:UA_ConnectionConfig; endpointUrl:UA_String; timeout:UA_UInt32; logger:PUA_Logger):UA_Connection; cdecl;

 {$IFDEF ENABLE_SERVER}
 PUA_ServerNetworkLayer = ^UA_ServerNetworkLayer;
 PUA_Server = ^UA_Server;
 UA_ServerNetworkLayer = record
     handle:Pointer; (* Internal data *)

     (* Points to external memory, i.e. handled by server or client *)
     statistics:PUA_NetworkStatistics;

     discoveryUrl:UA_String;

     localConnectionConfig:UA_ConnectionConfig;

     (* Start listening on the networklayer.
      *
      * @param nl The network layer
      * @return Returns UA_STATUSCODE_GOOD or an error code. *)
     start:function(nl:PUA_ServerNetworkLayer; const logger:PUA_Logger;
                            const customHostname:PUA_String):UA_StatusCode;cdecl;

     (* Listen for new and closed connections and arriving packets. Calls
      * UA_Server_processBinaryMessage for the arriving packets. Closed
      * connections are picked up here and forwarded to
      * UA_Server_removeConnection where they are cleaned up and freed.
      *
      * @param nl The network layer
      * @param server The server for processing the incoming packets and for
      *               closing connections.
      * @param timeout The timeout during which an event must arrive in
      *                milliseconds
      * @return A statuscode for the status of the network layer. *)
     listen:function(nl:PUA_ServerNetworkLayer; server:PUA_Server;
                             timeout:UA_UInt16):UA_StatusCode;cdecl;

     (* Close the network socket and all open connections. Afterwards, the
      * network layer can be safely deleted.
      *
      * @param nl The network layer
      * @param server The server that processes the incoming packets and for
      *               closing connections before deleting them.
      * @return A statuscode for the status of the closing operation. *)
     stop:procedure(nl:PUA_ServerNetworkLayer; server:PUA_Server);cdecl;

     (* Deletes the network layer context. Call only after stopping. *)
     clear:procedure(nl:PUA_ServerNetworkLayer);cdecl;
  end;
  {$ENDIF}

  { ------------------------ }
  { --- securitypolicy.h --- }
  { ------------------------ }
  PUA_SecurityPolicy = ^UA_SecurityPolicy;
  UA_SecurityPolicy = record
      (* Additional data *)
      policyContext: Pointer;

      (* The policy uri that identifies the implemented algorithms *)
      policyUri: UA_ByteString;

      (* The local certificate is specific for each SecurityPolicy since it
       * depends on the used key length. *)
      localCertificate: UA_ByteString;

      (* Function pointers grouped into modules *)
{ TODO:
      UA_SecurityPolicyAsymmetricModule asymmetricModule;
      UA_SecurityPolicySymmetricModule symmetricModule;
      UA_SecurityPolicySignatureAlgorithm certificateSigningAlgorithm;
      UA_SecurityPolicyChannelModule channelModule;
      UA_CertificateVerification *certificateVerification;
}
      logger: PUA_Logger;

      (* Updates the ApplicationInstanceCertificate and the corresponding private
       * key at runtime. *)
      updateCertificateAndPrivateKey: function(policy: PUA_SecurityPolicy;
                                               const newCertificate: UA_ByteString;
                                               const newPrivateKey: UA_ByteString): UA_StatusCode;

      (* Deletes the dynamic content of the policy *)
      deleteMembers: procedure(policy: PUA_SecurityPolicy);
  end;

  { -------------------- }
  { --- plugin/pki.h --- }
  { -------------------- }
  PUA_CertificateVerification  = ^UA_CertificateVerification;
  UA_CertificateVerification = record
      context : Pointer;
      verifyCertificate : function (verificationContext:Pointer; certificate:PUA_ByteString):UA_StatusCode; cdecl;
      verifyApplicationURI : function (verificationContext:Pointer; certificate:PUA_ByteString; applicationURI:PUA_String):UA_StatusCode; cdecl;
      deleteMembers : procedure (cv:PUA_CertificateVerification); cdecl;
  end;

  { ----------------------- }
  { --- client_config.h --- }
  { ----------------------- }
  UA_ClientConfig = record
      (* Basic client configuration *)
      clientContext: Pointer; (* User-defined data attached to the client *)
      logger: UA_Logger;   (* Logger used by the client *)
      timeout: UA_UInt32;  (* Response timeout in ms *)

      (* The description must be internally consistent.
       * - The ApplicationUri set in the ApplicationDescription must match the
       *   URI set in the server certificate *)
      clientDescription: UA_ApplicationDescription;

      (* Basic connection configuration *)
      userIdentityToken: UA_ExtensionObject; (* Configured User-Identity Token *)
      securityMode: UA_MessageSecurityMode;  (* None, Sign, SignAndEncrypt. The
                                              * default is invalid. This indicates
                                              * the client to select any matching
                                              * endpoint. *)
      securityPolicyUri: UA_String; (* SecurityPolicy for the SecureChannel. An
                                     * empty string indicates the client to select
                                     * any matching SecurityPolicy. *)

      (* Advanced connection configuration
       *
       * If either endpoint or userTokenPolicy has been set (at least one non-zero
       * byte in either structure), then the selected Endpoint and UserTokenPolicy
       * overwrite the settings in the basic connection configuration. The
       * userTokenPolicy array in the EndpointDescription is ignored. The selected
       * userTokenPolicy is set in the dedicated configuration field.
       *
       * If the advanced configuration is not set, the client will write to it the
       * selected Endpoint and UserTokenPolicy during GetEndpoints.
       *
       * The information in the advanced configuration is used during reconnect
       * when the SecureChannel was broken. *)
      endpoint: UA_EndpointDescription;
      userTokenPolicy: UA_UserTokenPolicy;

      (* Advanced client configuration *)

      secureChannelLifeTime: UA_UInt32; (* Lifetime in ms (then the channel needs
                                           to be renewed) *)
      requestedSessionTimeout: UA_UInt32; (* Session timeout in ms *)
      localConnectionConfig: UA_ConnectionConfig;
      connectivityCheckInterval: UA_UInt32 ;    (* Connectivity check interval in ms.
                                                 * 0 = background task disabled *)
      customDataTypes: ^UA_DataTypeArray; (* Custom DataTypes. Attention!
                                           * Custom datatypes are not cleaned
                                           * up together with the
                                           * configuration. So it is possible
                                           * to allocate them on ROM. *)

      (* Available SecurityPolicies *)
      securityPoliciesSize: Size_T;
      securityPolicies: ^UA_SecurityPolicy;

      (* Certificate Verification Plugin *)
      certificateVerification: UA_CertificateVerification;

      (* Callbacks for async connection handshakes *)
      initConnectionFunc: UA_ConnectClientConnection;
      pollConnectionFunc: function(Client: PUA_Client; Context: Pointer; Timeout: UA_UInt32): UA_StatusCode; cdecl;

      // Callback for state changes. The client state is differentated into the
      // SecureChannel state and the Session state. The connectStatus is set if
      // the client connection (including reconnects) has failed and the client
      // has to "give up". If the connectStatus is not set, the client still has
      // hope to connect or recover.
      stateCallback : procedure (Client: PUA_Client;
        ChannelState: UA_SecureChannelState;
        SessionState: UA_SessionState;
        ConnectStatus: UA_StatusCode); cdecl;

      (* When connectivityCheckInterval is greater than 0, every
       * connectivityCheckInterval (in ms), a async read request is performed on
       * the server. inactivityCallback is called when the client receive no
       * response for this read request The connection can be closed, this in an
       * attempt to recreate a healthy connection. *)
       inactivityCallback : procedure (Client: PUA_Client); cdecl;

    {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
      // Number of PublishResponse queued up in the server
      outStandingPublishRequests : UA_UInt16;

      (* If the client does not receive a PublishResponse after the defined delay
       * of ``(sub->publishingInterval * sub->maxKeepAliveCount) +
       * client->config.timeout)``, then subscriptionInactivityCallback is called
       * for the subscription.. *)
       subscriptionInactivityCallback : procedure (Client: PUA_Client; SubscriptionId: UA_UInt32; SubContext: Pointer);cdecl;
    {$ENDIF}
  end;
  PUA_ClientConfig = ^UA_ClientConfig;

  {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
  { ------------------------------ }
  { --- client_subscriptions.h --- }
  { ------------------------------ }
  // Callbacks defined for Subscriptions
  UA_Client_DeleteSubscriptionCallback = procedure(Client: PUA_Client; SubId: UA_UInt32; SubContext: Pointer); cdecl;
  UA_Client_StatusChangeNotificationCallback = procedure(Client: PUA_Client; SubId: UA_UInt32; SubContext: Pointer; Notification: PUA_StatusChangeNotification); cdecl;
  // Callback for the deletion of a MonitoredItem
  UA_Client_DeleteMonitoredItemCallback = procedure(Client: PUA_Client; SubId: UA_UInt32; SubContext: Pointer; MonId: UA_UInt32; MonContext: Pointer); cdecl;
  // Callback for DataChange notifications
  UA_Client_DataChangeNotificationCallback = procedure(Client: PUA_Client; SubId: UA_UInt32; SubContext: Pointer; MonId: UA_UInt32; MonContext: Pointer; value: PUA_DataValue); cdecl;
  // Callback for Event notifications
  UA_Client_EventNotificationCallback = procedure(Client: PUA_Client; SubId: UA_UInt32; SubContext: Pointer; MonId: UA_UInt32; MonContext: Pointer; NEventFields: Size_T; EventFields: PUA_Variant); cdecl;
  {$ENDIF}

  {$IFDEF ENABLE_SERVER}
  { ---------------- }
  { --- server.h --- }
  { ---------------- }
  UA_Server = record end;
  UA_MethodCallback = function (Server: PUA_Server;
    const SessionId: PUA_NodeId; SessionContext: Pointer;
    const MethodId: PUA_NodeId; MethodContext: Pointer;
    const ObjectId: PUA_NodeId; ObjectContext: Pointer;
    InputSize: SIZE_T; const Input: PUA_Variant;
    OutputSize: SIZE_T; Output: PUA_Variant): UA_StatusCode; cdecl;

  { ----------------------- }
  { --- server_config.h --- }
  { ----------------------- }
  UA_ServerConfig = record
    Logger: UA_Logger;
    (* Server Description:
     * The description must be internally consistent.
     * - The ApplicationUri set in the ApplicationDescription must match the
     *   URI set in the server certificate *)
    BuildInfo: UA_BuildInfo;
    ApplicationDescription: UA_ApplicationDescription;
    ServerCertificate: UA_ByteString;

    ShutdownDelay: UA_Double; (* Delay in ms from the shutdown signal (ctrl-c)
                                 until the actual shutdown. Clients need to be
                                 able to get a notification ahead of time. *)

    (* Rule Handling *)
    VerifyRequestTimestamp: UA_RuleHandling; (* Verify that the server sends a
                                              * timestamp in the request header *)
    AllowEmptyVariables: UA_RuleHandling; (* Variables (that don't have a
                                           * DataType of BaseDataType) must not
                                           * have an empty variant value. The
                                           * default behaviour is to auto-create
                                           * a matching zeroed-out value for
                                           * empty VariableNodes when they are
                                           * added. *)

    (* Custom DataTypes. Attention! Custom datatypes are not cleaned up together
     * with the configuration. So it is possible to allocate them on ROM. *)
    CustomDataTypes: PUA_DataTypeArray;

    (**
     * .. note:: See the section on :ref:`generic-types`. Examples for working
     *    with custom data types are provided in
     *    ``/examples/custom_datatype/``. *)

    (* Networking *)
    NetworkLayersSize: Size_T;
    NetworkLayers: PUA_ServerNetworkLayer;
    CustomHostname: UA_String;

    // FIXME: define the remaining fields
  end;
  PUA_ServerConfig = ^UA_ServerConfig;
  {$ENDIF ENABLE_SERVER}

const
  { ---------------- }
  { --- common.h --- }
  { ---------------- }

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

  { --------------------- }
  { --- statuscodes.h --- }
  { --------------------- }

  {$I statuscodes.inc}

  { ----------------- }
  { --- nodeids.h --- }
  { ----------------- }

  {$I nodeids.inc}

  { ------------------ }
  { --- ua_types.c --- }
  { ------------------ }
  UA_STRING_NULL: UA_String = (length: 0; data: nil);
  UA_BYTESTRING_NULL: UA_ByteString = (length: 0; data: nil);
  UA_GUID_NULL: UA_Guid = (data1: 0; data2: 0; data3: 0; data4: (0, 0, 0, 0, 0, 0, 0, 0));
  UA_NODEID_NULL: UA_NodeId = (namespaceIndex: 0; identifierType: UA_NODEIDTYPE_NUMERIC; identifier: (numeric: 0));

  { --------------- }
  { --- types.h --- }
  { --------------- }

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

  UA_Client_Subscriptions_create: function(client: PUA_Client; const request: UA_CreateSubscriptionRequest; subscriptionContext: Pointer; statusChangeCallback: UA_Client_StatusChangeNotificationCallback; deleteCallback: UA_Client_DeleteSubscriptionCallback): UA_CreateSubscriptionResponse; cdecl;
  UA_Client_Subscriptions_delete: function(client: PUA_Client; const request: UA_DeleteSubscriptionsRequest): UA_DeleteSubscriptionsResponse; cdecl;
  UA_Client_Subscriptions_deleteSingle: function(client: PUA_Client; subscriptionId: UA_UInt32): UA_StatusCode; cdecl;
  UA_Client_MonitoredItems_createDataChange: function(client: PUA_Client; subscriptionId: UA_UInt32; timestampsToReturn: UA_TimestampsToReturn; const item: UA_MonitoredItemCreateRequest; context: Pointer; callback: UA_Client_DataChangeNotificationCallback; deleteCallback: UA_Client_DeleteMonitoredItemCallback): UA_MonitoredItemCreateResult; cdecl;
  UA_Client_MonitoredItems_deleteSingle: function(client: PUA_Client; subscriptionId: UA_UInt32; monitoredItemId: UA_UInt32): UA_StatusCode; cdecl;

  {$IFDEF ENABLE_SERVER}
  UA_Server_new: function(): PUA_Server; cdecl;
  UA_ServerConfig_setMinimalCustomBuffer: function(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString; sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl;
  UA_Server_delete: procedure(server: PUA_Server); cdecl;
  UA_Server_getConfig: function(server: PUA_Server): PUA_ServerConfig; cdecl;
  UA_Server_run: function(server: PUA_Server; running: PUA_Boolean): UA_StatusCode; cdecl;
  UA_Server_run_startup: function(server: PUA_Server): UA_StatusCode; cdecl;
  UA_Server_run_iterate: function(server: PUA_Server; waitInternal: UA_Boolean):UA_UInt16; cdecl;
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
  __UA_Server_write: function (server: PUA_Server; const nodeId: PUA_NodeId; const attributeId: UA_AttributeId; const attr_type: PUA_DataType; attr: Pointer): UA_StatusCode; cdecl;
  UA_Server_addReference: function (server: PUA_Server; const sourceId:UA_NodeId;
                                    const refTypeId: UA_NodeId;
                                    const targetId: UA_ExpandedNodeId; isForward: UA_Boolean): UA_StatusCode; cdecl;
  UA_Server_deleteReference: function (server: PUA_Server; const sourceNodeId:UA_NodeId;
                                       const referenceTypeId: UA_NodeId; isForward: UA_Boolean;
                                       const targetNodeId: UA_ExpandedNodeId; deleteBitirectional: UA_Boolean): UA_StatusCode; cdecl;
  UA_Server_addNamespace: function (server: PUA_Server; namespace: PChar): UA_Uint16; cdecl;
  UA_Server_addMethodNodeEx: function(server: PUA_Server; const requestedNewNodeId:UA_NodeId;
                            const parentNodeId:UA_NodeId;
                            const referenceTypeId:UA_NodeId;
                            const browseName:UA_QualifiedName ;
                            const attr:UA_MethodAttributes; method:UA_MethodCallback;
                            inputArgumentsSize:SIZE_T; const inputArguments: PUA_Argument;
                            const inputArgumentsRequestedNewNodeId:UA_NodeId;
                            inputArgumentsOutNewNodeId:PUA_NodeId;
                            outputArgumentsSize:SIZE_T; const outputArguments:PUA_Argument;
                            const outputArgumentsRequestedNewNodeId:UA_NodeId;
                            outputArgumentsOutNewNodeId:PUA_NodeId;
                            nodeContext: Pointer; outNewNodeId:PUA_NodeId): UA_StatusCode; cdecl;
  UA_MethodAttributes_default:UA_MethodAttributes;
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

{ ---------------- }
{ --- client.h --- }
{ ---------------- }

(**
 * .. _client:
 *
 * Client
 * ======
 *
 * The client implementation allows remote access to all OPC UA services. For
 * convenience, some functionality has been wrapped in :ref:`high-level
 * abstractions <client-highlevel>`.
 *
 * **However**: At this time, the client does not yet contain its own thread or
 * event-driven main-loop. So the client will not perform any actions
 * automatically in the background. This is especially relevant for
 * subscriptions. The user will have to periodically call
 * `UA_Client_Subscriptions_manuallySendPublishRequest`. See also :ref:`here
 * <client-subscriptions>`.
 *
 *
 * .. include:: client_config.rst
 *
 * Client Lifecycle
 * ---------------- *)

(* Create a new client *)
function UA_Client_new(): PUA_Client; cdecl; external libopen62541;

(* Creates a new client. Moves the config into the client with a shallow copy.
 * The config content is cleared together with the client. *)
function UA_Client_newWithConfig(const config: PUA_ClientConfig): PUA_Client; cdecl; external libopen62541;

(* Returns the current state. All arguments except ``client`` can be NULL. *)
procedure UA_Client_getState(client: PUA_Client; channelState: PUA_SecureChannelState; sessionState: PUA_SessionState; connectStatus: PUA_StatusCode); cdecl; external libopen62541;

(* Get the client configuration *)
function UA_Client_getConfig(client: PUA_Client): PUA_ClientConfig; cdecl; external libopen62541;

function UA_ClientConfig_setDefault(config: PUA_ClientConfig): UA_StatusCode; cdecl; external libopen62541;
{$IFDEF UA_ENABLE_ENCRYPTION}
function UA_ClientConfig_setDefaultEncryption(config: PUA_ClientConfig; localCertificate, privateKey: UA_ByteString; trustList: PUA_ByteString; trustListSize: Size_T; revocationList: PUA_ByteString; revocationListSize: Size_T): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}

(* Delete a client *)
procedure UA_Client_delete(client: PUA_Client); cdecl; external libopen62541;

(* Connect to the server
 *
 * @param client to use
 * @param endpointURL to connect (for example "opc.tcp://localhost:4840")
 * @return Indicates whether the operation succeeded or returns an error code *)
function UA_Client_connect(client: PUA_Client; const endpointUrl: AnsiString): UA_StatusCode; cdecl; external libopen62541;
(* Connect to the selected server with the given username and password *)
//function UA_Client_connect_username(client: PUA_Client; endpointUrl: PAnsiChar; username, password: PAnsiChar): UA_StatusCode; cdecl; external libopen62541; deprecated;
(* Disconnect and close a connection to the selected server *)
function UA_Client_disconnect(client: PUA_Client): UA_StatusCode; cdecl; external libopen62541;

(**
 * .. _client-services:
 *
 * Services
 * --------
 *
 * The raw OPC UA services are exposed to the client. But most of them time, it
 * is better to use the convenience functions from ``ua_client_highlevel.h``
 * that wrap the raw services. *)
(* Don't use this function. Use the type versions below instead. *)
procedure __UA_Client_Service(client: PUA_Client; const request: Pointer; const requestType: PUA_DataType; response: Pointer; const responseType: PUA_DataType); cdecl; external libopen62541;

(*
 * Asynchronous Services
 * ---------------------
 * All OPC UA services are asynchronous in nature. So several service calls can
 * be made without waiting for a response first. Responess may come in a
 * different ordering. *)
(* Listen on the network and process arriving asynchronous responses in the
  * background. Internal housekeeping, renewal of SecureChannels and subscription
  * management is done as well. *)
function UA_Client_run_iterate(client: PUA_Client; timeout: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;


{ --------------- }
{ --- types.h --- }
{ --------------- }
(* Returns the human-readable name of the StatusCode. If no matching StatusCode
 * is found, a default string for "Unknown" is returned. This feature might be
 * disabled to create a smaller binary with the
 * UA_ENABLE_STATUSCODE_DESCRIPTIONS build-flag. Then the function returns an
 * empty string for every StatusCode. *)
function UA_StatusCode_name(code: UA_StatusCode): PAnsiChar; cdecl; external libopen62541;

(* Copies the content on the heap. Returns a null-string when alloc fails *)
function UA_String_fromChars(src: PAnsiChar): UA_String; cdecl; external libopen62541;
(* Check for equality *)
function UA_String_equal(const s1, s2: PUA_String): UA_Boolean; cdecl; external libopen62541;
function UA_NodeId_isNull(p: PUA_NodeId): UA_Boolean; cdecl; external libopen62541;
(* Print the NodeId in the human-readable format *)
function UA_NodeId_print(id: PUA_NodeId; output: PUA_String): UA_StatusCode; cdecl; external libopen62541;
function UA_NumericRange_parse(range: PUA_NumericRange; const str: UA_String): UA_StatusCode; cdecl; external libopen62541;
(* Set the variant to a scalar value that already resides in memory. The value takes on the lifecycle of the variant and is deleted with it. *)
procedure UA_Variant_setScalar(v: PUA_Variant; p: Pointer; _type: PUA_DataType); cdecl; external libopen62541;
(* Set the variant to a scalar value that is copied from an existing variable. *)
function UA_Variant_setScalarCopy(v: PUA_Variant; p: Pointer; _type: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
(* Set the variant to an array that already resides in memory. The array takes on the lifecycle of the variant and is deleted with it. *)
procedure UA_Variant_setArray(v: PUA_Variant; arrayData: Pointer; arraySize: Size_T; _type: PUA_DataType); cdecl; external libopen62541;
(* Set the variant to an array that is copied from an existing array. *)
function UA_Variant_setArrayCopy(v: PUA_Variant; arrayData: Pointer; arraySize: Size_T; _type: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
(* Insert a range of data into an existing variant. The data array can't be reused afterwards if it contains types without a fixed size (e.g. strings)
 * since the members are moved into the variant and take on its lifecycle. *)
function UA_Variant_setRange(v: PUA_Variant; arrayData: Pointer; arraySize: Size_T; const range: UA_NumericRange): UA_StatusCode; cdecl; external libopen62541;
(* Deep-copy a range of data into an existing variant. *)
function UA_Variant_setRangeCopy(v: PUA_Variant; arrayData: Pointer; arraySize: Size_T; const range: UA_NumericRange): UA_StatusCode; cdecl; external libopen62541;

function UA_DateTime_toStruct(t: UA_DateTime): UA_DateTimeStruct; cdecl; external libopen62541;
function UA_DateTime_fromStruct(ts: UA_DateTimeStruct): UA_DateTime; cdecl; external libopen62541;

(* Returns the data type description for the type's identifier or NULL if no matching data type was found. *)
function UA_findDataType(typeId: PUA_NodeId): PUA_DataType; cdecl; external libopen62541;

(* Allocates and initializes a variable of type dataType *)
function UA_new(const _type: PUA_DataType): Pointer; cdecl; external libopen62541;
(* Copies the content of two variables. *)
function UA_copy(src,dst: Pointer; const _type: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
(* Deletes the dynamically allocated content of a variable (e.g. resets all
 * arrays to undefined arrays). Afterwards, the variable can be safely deleted
 * without causing memory leaks. But the variable is not initialized and may
 * contain old data that is not memory-relevant. *)
procedure UA_clear(p: Pointer; const _type: PUA_DataType); cdecl; external libopen62541;
(* Frees a variable and all of its content. *)
procedure UA_delete(p: Pointer; const _type: PUA_DataType); cdecl; external libopen62541;
(* Deletes an array. *)
procedure UA_Array_delete(p: Pointer; size: Size_T; const _type: PUA_DataType); cdecl; external libopen62541;

{ -------------------------- }
{ --- client_highlevel.h --- }
{ -------------------------- }
function __UA_Client_readAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _out: Pointer; outDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_readArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outArrayDimensionsSize: Size_T; out outArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;

function __UA_Client_writeAttribute(client: PUA_Client; const nodeId: PUA_NodeId; attributeId: UA_AttributeId; _in: Pointer; inDataType: PUA_DataType): UA_StatusCode; cdecl; external libopen62541;
function UA_Client_writeArrayDimensionsAttribute(client: PUA_Client; const nodeId: UA_NodeId; newArrayDimensionsSize: Size_T; newArrayDimensions: PUA_UInt32): UA_StatusCode; cdecl; external libopen62541;
{$IFDEF UA_ENABLE_METHODCALLS}
function UA_Client_call(client: PUA_Client; const objectId, methodId: UA_NodeId; inputSize: Size_T; input: PUA_Variant; out outputSize: Size_T; out output: PUA_Variant): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}

{ ------------------------------- }
{ --- plugin/securitypolicy.h --- }
{ ------------------------------- }
function UA_SecurityPolicy_None(policy: PUA_SecurityPolicy; certificateVerification: PUA_CertificateVerification; const localCertificate: UA_ByteString; const logger: PUA_Logger): UA_StatusCode; cdecl; external libopen62541;

{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
{ ------------------------------ }
{ --- client_subscriptions.h --- }
{ ------------------------------ }
(**
 * .. _client-subscriptions:
 *
 * Subscriptions
 * -------------
 *
 * Subscriptions in OPC UA are asynchronous. That is, the client sends several
 * PublishRequests to the server. The server returns PublishResponses with
 * notifications. But only when a notification has been generated. The client
 * does not wait for the responses and continues normal operations.
 *
 * Note the difference between Subscriptions and MonitoredItems.
 * Subscriptions are used to report back notifications.
 * MonitoredItems are used to generate notifications.
 * Every MonitoredItem is attached to exactly one Subscription.
 * And a Subscription can contain many MonitoredItems.
 *
 * The client automatically processes PublishResponses (with a callback) in the
 * background and keeps enough PublishRequests in transit. The PublishResponses
 * may be recieved during a synchronous service call or in
 * ``UA_Client_runAsync``. *)
function UA_Client_Subscriptions_create(client: PUA_Client; const request: UA_CreateSubscriptionRequest; subscriptionContext: Pointer; statusChangeCallback: UA_Client_StatusChangeNotificationCallback; deleteCallback: UA_Client_DeleteSubscriptionCallback): UA_CreateSubscriptionResponse; cdecl; external libopen62541;
function UA_Client_Subscriptions_delete(client: PUA_Client; const request: UA_DeleteSubscriptionsRequest): UA_DeleteSubscriptionsResponse; cdecl; external libopen62541;
(* Delete a single subscription *)
function UA_Client_Subscriptions_deleteSingle(client: PUA_Client; subscriptionId: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;

(**
 * MonitoredItems
 * --------------
 *
 * MonitoredItems for Events indicate the ``EventNotifier`` attribute. This
 * indicates to the server not to monitor changes of the attribute, but to
 * forward Event notifications from that node.
 *
 * During the creation of a MonitoredItem, the server may return changed
 * adjusted parameters. Check the returned ``UA_CreateMonitoredItemsResponse``
 * to get the current parameters. *)
(*
 * Clients define MonitoredItems to subscribe to data and Events.
 * Each MonitoredItem identifies the item to be monitored and the Subscription are used to report Notifications to the Client.
 * The item to be monitored may be any Node Attribute.
 *)
(* Don't use to monitor the EventNotifier attribute *)
function UA_Client_MonitoredItems_createDataChanges(client: PUA_Client; const request: UA_CreateMonitoredItemsRequest; contexts: PPointer; callbacks: UA_Client_DataChangeNotificationCallback; deleteCallbacks: UA_Client_DeleteMonitoredItemCallback): UA_CreateMonitoredItemsResponse; cdecl; external libopen62541;
function UA_Client_MonitoredItems_createDataChange(client: PUA_Client; subscriptionId: UA_UInt32; timestampsToReturn: UA_TimestampsToReturn; const item: UA_MonitoredItemCreateRequest; context: Pointer; callback: UA_Client_DataChangeNotificationCallback; deleteCallback: UA_Client_DeleteMonitoredItemCallback): UA_MonitoredItemCreateResult; cdecl; external libopen62541;
function UA_Client_MonitoredItems_deleteSingle(client: PUA_Client; subscriptionId: UA_UInt32; monitoredItemId: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}


{$IFDEF ENABLE_SERVER}
{ ------------------------------- }
{ --- server_config_default.h --- }
{ ------------------------------- }

(* Create a new server with default plugins for logging etc. used during
 * initialization. No network layer and SecurityPolicies are set so far. *)
function UA_Server_new(): PUA_Server; cdecl; external libopen62541;
function UA_ServerConfig_setMinimalCustomBuffer(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString; sendBufferSize, recvBufferSize: UA_UInt32): UA_StatusCode; cdecl; external libopen62541;

{ ---------------- }
{ --- server.h --- }
{ ---------------- }
procedure UA_Server_delete(server: PUA_Server); cdecl; external libopen62541;
function UA_Server_getConfig(server: PUA_Server): PUA_ServerConfig; cdecl; external libopen62541;

(* Runs the main loop of the server. In each iteration, this calls into the
 * networklayers to see if messages have arrived.
 *
 * @param server The server object.
 * @param running The loop is run as long as *running is true.
 *        Otherwise, the server shuts down.
 * @return Returns the statuscode of the UA_Server_run_shutdown method *)
function UA_Server_run(server: PUA_Server; running: PUA_Boolean): UA_StatusCode; cdecl; external libopen62541;

(* The prologue part of UA_Server_run (no need to use if you call UA_Server_run) *)
function UA_Server_run_startup(server: PUA_Server): UA_StatusCode; cdecl; external libopen62541;
(* Executes a single iteration of the server's main loop. *)
function UA_Server_run_iterate(server: PUA_Server; waitInternal: UA_Boolean):UA_UInt16; cdecl; external libopen62541;
(* The epilogue part of UA_Server_run (no need to use if you call UA_Server_run) *)
function UA_Server_run_shutdown(server: PUA_Server): UA_StatusCode; cdecl; external libopen62541;

function __UA_Server_addNode(server: PUA_Server; const nodeClass: UA_NodeClass;
                    const requestedNewNodeId: PUA_NodeId;
                    const parentNodeId: PUA_NodeId;
                    const referenceTypeId: PUA_NodeId;
                    const browseName: UA_QualifiedName;
                    const typeDefinition: PUA_NodeId;
                    const attr: PUA_NodeAttributes;
                    const attributeType: PUA_DataType;
                    nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode; cdecl;  external libopen62541;
function UA_Server_addReference(server: PUA_Server; const sourceId:UA_NodeId;
                                const refTypeId: UA_NodeId;
                                const targetId: UA_ExpandedNodeId; isForward: UA_Boolean): UA_StatusCode; cdecl; external libopen62541;
function UA_Server_deleteReference(server: PUA_Server; const sourceNodeId:UA_NodeId;
                                   const referenceTypeId: UA_NodeId; isForward: UA_Boolean;
                                   const targetNodeId: UA_ExpandedNodeId; deleteBitirectional: UA_Boolean): UA_StatusCode; cdecl; external libopen62541;
function __UA_Server_write(server: PUA_Server; const nodeId: PUA_NodeId; const attributeId: UA_AttributeId; const attr_type: PUA_DataType; attr: Pointer): UA_StatusCode; cdecl; external libopen62541;
function UA_Server_addNamespace: function (server: PUA_Server; namespace: PChar):UA_Uint16; cdecl; external libopen62541;
function UA_Server_addMethodNodeEx: function(server: PUA_Server; const requestedNewNodeId:PUA_NodeId;
                          const parentNodeId:PUA_NodeId;
                          const referenceTypeId:PUA_NodeId;
                          const browseName:PUA_QualifiedName ;
                          const attr:PUA_MethodAttributes; method:UA_MethodCallback;
                          inputArgumentsSize:SIZE_T; const inputArguments: PUA_Argument;
                          const inputArgumentsRequestedNewNodeId:PUA_NodeId;
                          inputArgumentsOutNewNodeId:PUA_NodeId;
                          outputArgumentsSize:SIZE_T; const outputArguments:PUA_Argument;
                          const outputArgumentsRequestedNewNodeId:PUA_NodeId;
                          outputArgumentsOutNewNodeId:PUA_NodeId;
                          nodeContext: Pointer; outNewNodeId:PUA_NodeId): UA_StatusCode; cdecl;  external libopen62541;
{$ENDIF}
{$ENDIF}

{ --------------- }
{ --- types.h --- }
{ --------------- }
{ the non allocating versions use 'var' to emphasize that not a copy but the
  original value is used (and it shouldn't be changed)  and to make it
  impossible to use a property as an actual parameter }
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

(* my helper functions *)
function UA_StringToStr(const s: UA_String): AnsiString;
function UA_LocalizedTextToStr(const t: UA_LocalizedText): AnsiString;
function UA_NodeIdToStr(const id: UA_NodeId): AnsiString;
function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const indexRange: AnsiString; out outValue: UA_Variant): UA_StatusCode; overload;

(* Returns true if the variant has no value defined (contains neither an array nor a scalar value). *)
function UA_Variant_isEmpty(const v: PUA_Variant): Boolean;
(* Returns true if the variant contains a scalar value. Note that empty variants contain an array of length -1 (undefined). *)
function UA_Variant_isScalar(const v: PUA_Variant): Boolean;
(* Returns true if the variant contains a scalar value of the given type. *)
function UA_Variant_hasScalarType(const v: PUA_Variant; const _type: PUA_DataType): Boolean;
(* Returns true if the variant contains an array of the given type. *)
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

(* The following functions are shorthand for creating NodeIds. *)
function UA_NODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_UInt32): UA_NodeId;
function UA_NODEID_STRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
function UA_NODEID_STRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
function UA_NODEID_GUID(nsIndex: UA_UInt16; guid: UA_Guid): UA_NodeId;
function UA_NODEID_BYTESTRING(nsIndex: UA_UInt16; var chars: AnsiString): UA_NodeId;
function UA_NODEID_BYTESTRING_ALLOC(nsIndex: UA_UInt16; const chars: AnsiString): UA_NodeId;
function UA_EXPANDEDNODEID_NUMERIC(nsIndex: UA_UInt16; identifier: UA_Uint32): UA_ExpandedNodeId;

(* Test if the data type is a numeric builtin data type. This includes Boolean,
 * integers and floating point numbers. Not included are DateTime and StatusCode. *)
//function UA_DataType_isNumeric(_type: PUA_DataType): UA_Boolean; cdecl; external libopen62541;
(* Initializes a variable to default values *)
procedure UA_init(p: Pointer; const _type: PUA_DataType);

{ ---------------------------------- }
{ --- types_generated_handling.h --- }
{ ---------------------------------- }
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

{ ---------------- }
{ --- client.h --- }
{ ---------------- }
function UA_Client_connect_username(client: PUA_Client; const endpointUrl, username, password: AnsiString): UA_StatusCode; deprecated;
function UA_Client_connectUsername(client: PUA_Client; const endpointUrl, username, password: AnsiString): UA_StatusCode;
function UA_Client_Service_read(client: PUA_Client; const request: UA_ReadRequest): UA_ReadResponse;
function UA_Client_Service_browse(client: PUA_Client; const request: UA_BrowseRequest): UA_BrowseResponse;

{ -------------------------- }
{ --- client_highlevel.h --- }
{ -------------------------- }
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: UA_Variant): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: Byte): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: Smallint): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: Longint): UA_StatusCode; overload;
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: AnsiString): UA_StatusCode; overload;
function UA_Client_readDataTypeAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outDataType: UA_NodeId): UA_StatusCode;
function UA_Client_readValueRankAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValueRank: UA_Int32): UA_StatusCode;
function UA_Client_readBrowseNameAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outBrowseName: UA_QualifiedName): UA_StatusCode;
function UA_Client_readDisplayNameAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outDisplayName: UA_LocalizedText): UA_StatusCode;
function UA_Client_readDescriptionAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outDescription: UA_LocalizedText): UA_StatusCode;

function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newValue: UA_Variant): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: Byte): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: Smallint): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: Longint): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: AnsiString): UA_StatusCode; overload;
function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValues: array of AnsiString): UA_StatusCode; overload;
function UA_Client_writeDescriptionAttribute(client: PUA_Client; const nodeId: UA_NodeId; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newDescription: UA_LocalizedText): UA_StatusCode;
function UA_Client_writeDataTypeAttribute(client: PUA_Client; const nodeId: UA_NodeId; newDataType: PUA_NodeId): UA_StatusCode;
function UA_Client_writeValueRankAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValueRank: UA_Int32): UA_StatusCode;

{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
function UA_MonitoredItemCreateRequest_default(NodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
{$ENDIF}

{$IFDEF ENABLE_SERVER}
{ ------------------------------- }
{ --- server_config_default.h --- }
{ ------------------------------- }
function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;
{ ---------------- }
{ --- server.h --- }
{ ---------------- }
function UA_Server_addVariableNode(server: PUA_Server; const requestedNewNodeId: UA_NodeId;
                          const parentNodeId: UA_NodeId;
                          const referenceTypeId: UA_NodeId;
                          const browseName: UA_QualifiedName;
                          const typeDefinition: UA_NodeId;
                          const attr: UA_VariableAttributes;
                          nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
function UA_Server_addObjectTypeNode(server:PUA_Server; const requestedNewNodeId:UA_NodeId;
                            const parentNodeId:UA_NodeId;
                            const referenceTypeId:UA_NodeId;
                            const browseName: UA_QualifiedName;
                            const attr:UA_ObjectTypeAttributes;
                            nodeContext:Pointer; outNewNodeId: PUA_NodeId):UA_StatusCode;
function UA_Server_addObjectNode(server:PUA_Server; const requestedNewNodeId:UA_NodeId;
                        const parentNodeId:UA_NodeId;
                        const referenceTypeId:UA_NodeId;
                        const browseName: UA_QualifiedName;
                        const typeDefinition: UA_NodeId;
                        const attr:UA_ObjectAttributes;
                        nodeContext:Pointer; outNewNodeId: PUA_NodeId):UA_StatusCode;
function UA_Server_addVariableTypeNode(server:PUA_Server;
                              const requestedNewNodeId:UA_NodeId;
                              const parentNodeId:UA_NodeId;
                              const referenceTypeId:UA_NodeId;
                              const browseName:UA_QualifiedName;
                              const typeDefinition:UA_NodeId;
                              const attr:UA_VariableTypeAttributes;
                              nodeContext:Pointer; outNewNodeId:PUA_NodeId):UA_StatusCode;
function UA_Server_addViewNode(server:PUA_Server; const requestedNewNodeId:UA_NodeId;
                      const parentNodeId:UA_NodeId;
                      const referenceTypeId:UA_NodeId;
                      const browseName:UA_QualifiedName;
                      const attr:UA_ViewAttributes;
                      nodeContext:Pointer; outNewNodeId:PUA_NodeId):UA_StatusCode;
function UA_Server_addReferenceTypeNode(server:PUA_Server;
                               const requestedNewNodeId:UA_NodeId;
                               const parentNodeId:UA_NodeId;
                               const referenceTypeId:UA_NodeId;
                               const browseName:UA_QualifiedName;
                               const attr:UA_ReferenceTypeAttributes;
                               nodeContext:Pointer; outNewNodeId:PUA_NodeId):UA_StatusCode;
function UA_Server_addDataTypeNode(Server:PUA_Server;
                          const RequestedNewNodeId:UA_NodeId;
                          const ParentNodeId:UA_NodeId;
                          const ReferenceTypeId:UA_NodeId;
                          const BrowseName:UA_QualifiedName;
                          const Attr:UA_DataTypeAttributes;
                          NodeContext:Pointer; OutNewNodeId:PUA_NodeId):UA_StatusCode;
function UA_Server_writeValue(server: PUA_Server; const nodeId: UA_NodeId; const Value: UA_Variant): UA_StatusCode;
function UA_Server_addMethodNode(Server: PUA_Server; const requestedNewNodeId:UA_NodeId;
                            const parentNodeId:UA_NodeId;
                            const referenceTypeId:UA_NodeId;
                            const browseName:UA_QualifiedName ;
                            const attr:UA_MethodAttributes; method:UA_MethodCallback;
                            inputArgumentsSize:SIZE_T; const inputArguments: PUA_Argument;
                            outputArgumentsSize:SIZE_T; const outputArguments:PUA_Argument;
                            nodeContext: Pointer; outNewNodeId:PUA_NodeId): UA_StatusCode;
{$ENDIF}

implementation

{$IFDEF LOAD_DYNAMICALLY}
uses
  SysUtils,{$IFDEF MSWINDOWS}Windows{$ELSE}DynLibs{$ENDIF};

var
  open62541LibHandle: {$IFDEF MSWINDOWS}THandle{$ELSE}TLibHandle{$ENDIF};
  RefCount: integer;

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
  if RefCount = 1 then begin
    open62541LibHandle := LoadLibrary(libopen62541);
    if open62541LibHandle = 0 then begin
      RefCount := 0;
      raise EInOutError.CreateFmt('Can not load library "%s". Check your installation.'+sLineBreak+'%s',
                                  [libopen62541, GetLoadErrorStr()]);
    end;

    Pointer(UA_TYPES) := GetProcedureAddress(open62541LibHandle,'UA_TYPES'); // external variable name
    UA_VariableAttributes_default := PUA_VariableAttributes(GetProcedureAddress(open62541LibHandle,'UA_VariableAttributes_default'))^;
    UA_MethodAttributes_default := PUA_MethodAttributes(GetProcedureAddress(open62541LibHandle,'UA_MethodAttributes_default'))^;
    UA_ObjectAttributes_default := PUA_ObjectAttributes(GetProcedureAddress(open62541LibHandle,'UA_ObjectAttributes_default'))^;
    UA_ObjectTypeAttributes_default := PUA_ObjectTypeAttributes(GetProcedureAddress(open62541LibHandle,'UA_ObjectTypeAttributes_default'))^;
    UA_ReferenceTypeAttributes_default := PUA_ReferenceTypeAttributes(GetProcedureAddress(open62541LibHandle,'UA_ReferenceTypeAttributes_default'))^;
    UA_DataTypeAttributes_default := PUA_DataTypeAttributes(GetProcedureAddress(open62541LibHandle,'UA_DataTypeAttributes_default'))^;

    @UA_Client_new := GetProcedureAddress(open62541LibHandle,'UA_Client_new');
    @UA_Client_newWithConfig := GetProcedureAddress(open62541LibHandle,'UA_Client_newWithConfig');
    @UA_Client_getState := GetProcedureAddress(open62541LibHandle,'UA_Client_getState');
    @UA_Client_getConfig := GetProcedureAddress(open62541LibHandle,'UA_Client_getConfig');
    @UA_ClientConfig_setDefault := GetProcedureAddress(open62541LibHandle,'UA_ClientConfig_setDefault');
    @UA_ClientConfig_setDefaultEncryption := GetProcedureAddress(open62541LibHandle,'UA_ClientConfig_setDefaultEncryption');
    {$IFDEF UA_VER1_3}
    @UA_CertificateVerification_AcceptAll := GetProcedureAddress(open62541LibHandle,'UA_CertificateVerification_AcceptAll');
    {$ENDIF}
    @UA_Client_delete := GetProcedureAddress(open62541LibHandle,'UA_Client_delete');
    @UA_StatusCode_name := GetProcedureAddress(open62541LibHandle,'UA_StatusCode_name');
    @UA_Client_connect := GetProcedureAddress(open62541LibHandle,'UA_Client_connect');
    @UA_Client_disconnect := GetProcedureAddress(open62541LibHandle,'UA_Client_disconnect');
    @__UA_Client_Service := GetProcedureAddress(open62541LibHandle,'__UA_Client_Service');
    @UA_Client_run_iterate := GetProcedureAddress(open62541LibHandle,'UA_Client_run_iterate');

    @UA_String_fromChars := GetProcedureAddress(open62541LibHandle,'UA_String_fromChars');
    @UA_String_equal := GetProcedureAddress(open62541LibHandle,'UA_String_equal');
    @UA_NodeId_isNull := GetProcedureAddress(open62541LibHandle,'UA_NodeId_isNull');
    @UA_NodeId_print := GetProcedureAddress(open62541LibHandle,'UA_NodeId_print');
    @UA_NumericRange_parse := GetProcedureAddress(open62541LibHandle,'UA_NumericRange_parse');
    @UA_Variant_setScalar := GetProcedureAddress(open62541LibHandle,'UA_Variant_setScalar');
    @UA_Variant_setScalarCopy := GetProcedureAddress(open62541LibHandle,'UA_Variant_setScalarCopy');
    @UA_Variant_setArray := GetProcedureAddress(open62541LibHandle,'UA_Variant_setArray');
    @UA_Variant_setArrayCopy := GetProcedureAddress(open62541LibHandle,'UA_Variant_setArrayCopy');
    @UA_DateTime_toStruct := GetProcedureAddress(open62541LibHandle,'UA_DateTime_toStruct');
    @UA_DateTime_fromStruct := GetProcedureAddress(open62541LibHandle,'UA_DateTime_fromStruct');
    @UA_findDataType := GetProcedureAddress(open62541LibHandle,'UA_findDataType');

    @UA_new := GetProcedureAddress(open62541LibHandle,'UA_new');
    @UA_copy := GetProcedureAddress(open62541LibHandle,'UA_copy');
    @UA_clear := GetProcedureAddress(open62541LibHandle,'UA_clear');
    @UA_delete := GetProcedureAddress(open62541LibHandle,'UA_delete');
    @UA_Array_delete := GetProcedureAddress(open62541LibHandle, 'UA_Array_delete');

    @__UA_Client_readAttribute := GetProcedureAddress(open62541LibHandle,'__UA_Client_readAttribute');
    @UA_Client_readArrayDimensionsAttribute := GetProcedureAddress(open62541LibHandle,'UA_Client_readArrayDimensionsAttribute');
    @__UA_Client_writeAttribute := GetProcedureAddress(open62541LibHandle,'__UA_Client_writeAttribute');
    @UA_Client_writeArrayDimensionsAttribute := GetProcedureAddress(open62541LibHandle,'UA_Client_writeArrayDimensionsAttribute');
    @UA_Client_call := GetProcedureAddress(open62541LibHandle,'UA_Client_call');

    @UA_Client_Subscriptions_create := GetProcedureAddress(open62541LibHandle,'UA_Client_Subscriptions_create');
    @UA_Client_Subscriptions_delete := GetProcedureAddress(open62541LibHandle,'UA_Client_Subscriptions_delete');
    @UA_Client_Subscriptions_deleteSingle := GetProcedureAddress(open62541LibHandle,'UA_Client_Subscriptions_deleteSingle');
    @UA_Client_MonitoredItems_createDataChange := GetProcedureAddress(open62541LibHandle,'UA_Client_MonitoredItems_createDataChange');
    @UA_Client_MonitoredItems_deleteSingle := GetProcedureAddress(open62541LibHandle,'UA_Client_MonitoredItems_deleteSingle');

    @UA_Server_new := GetProcedureAddress(open62541LibHandle,'UA_Server_new');
    @UA_ServerConfig_setMinimalCustomBuffer := GetProcedureAddress(open62541LibHandle,'UA_ServerConfig_setMinimalCustomBuffer');
    @UA_Server_delete := GetProcedureAddress(open62541LibHandle,'UA_Server_delete');
    @UA_Server_getConfig := GetProcedureAddress(open62541LibHandle,'UA_Server_getConfig');
    @UA_Server_run := GetProcedureAddress(open62541LibHandle,'UA_Server_run');
    @UA_Server_run_startup := GetProcedureAddress(open62541LibHandle,'UA_Server_run_startup');
    @UA_Server_run_iterate := GetProcedureAddress(open62541LibHandle,'UA_Server_run_iterate');
    @UA_Server_run_shutdown := GetProcedureAddress(open62541LibHandle,'UA_Server_run_shutdown');

    @__UA_Server_addNode := GetProcedureAddress(open62541LibHandle,'__UA_Server_addNode');
    @UA_Server_addReference:= GetProcedureAddress(open62541LibHandle,'UA_Server_addReference');
    @UA_Server_deleteReference:= GetProcedureAddress(open62541LibHandle,'UA_Server_deleteReference');
    @__UA_Server_write := GetProcedureAddress(open62541LibHandle,'__UA_Server_write');
    @UA_Server_addNamespace := GetProcedureAddress(open62541LibHandle,'UA_Server_addNamespace');
    @UA_Server_addMethodNodeEx := GetProcedureAddress(open62541LibHandle,'UA_Server_addMethodNodeEx');
  end;
end;

procedure UnloadOpen62541;
begin
  if RefCount > 0 then begin
    Dec(RefCount);
    if RefCount = 0 then begin
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


(**
 * ``UA_STRING`` returns a string pointing to the original char-array.
 * ``UA_STRING_ALLOC`` is shorthand for ``UA_String_fromChars`` and makes a copy
 * of the char-array. *)
function _UA_STRING(var chars: AnsiString): UA_String; inline;
begin
  if chars='' then begin
    Result.Length := 0;
    Result.Data := nil;
  end
  else begin
    Result.Length := Length(chars);
    Result.Data := @chars[1];
  end;
end;

function _UA_STRING_ALLOC(const chars: AnsiString): UA_String; inline;
var bs:UA_BYTESTRING;
begin
  if chars='' then
begin
    result.Length:=0;
    result.Data:=Nil;
  end else
  begin
    //this contortion is necessary in order to let the library allocate the memory
    //(the C and pascal alloc/free cannot be mixed)
    bs.Length:=length(chars);
    bs.Data:=@chars[1];
    UA_copy(@bs, @result, @UA_TYPES[UA_TYPES_BYTESTRING]);
  end;
end;

function _UA_BYTESTRING(var chars: AnsiString): UA_ByteString;
begin
  result:=UA_ByteString(_UA_STRING(chars));
end;

function _UA_BYTESTRING_ALLOC(const chars: AnsiString): UA_ByteString; inline;
begin
  result:=UA_ByteString(_UA_STRING_ALLOC(chars));
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
  uas:UA_String;
begin
  Result.dimensionsSize := 0;
  Result.dimensions := nil;
  uas:=_UA_STRING_ALLOC(s);
  UA_NumericRange_parse(@Result, uas);
  UA_String_clear(uas);
end;

function _UA_String_equal(const s1: UA_String; const s2: AnsiString):boolean; overload;
var s: UA_String;
begin
  s := _UA_STRING_ALLOC(s2);
  Result:=UA_String_equal(@s1,@s);
  UA_String_clear(s);
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
var output: UA_String;
begin
  output := UA_STRING_NULL;
  UA_NodeId_print(@id, @output);
  SetString(Result, PAnsiChar(output.Data), output.Length);
  UA_String_Clear(output);
end;

function UA_DataTypeToStr(typeId: UA_NodeId): AnsiString;
var pDataType: PUA_DataType;
begin
  pDataType := UA_findDataType(@typeId);
  if pDataType = nil then Result:='Unknown' else Result:=pDataType^.TypeName;
end;

{$WARN 5058 off : Variable "$1" does not seem to be initialized}
procedure UA_Variant_init(out p: UA_Variant);
begin
  FillChar(p, SizeOf(UA_Variant), #0);
end;
{$WARN 5058 on : Variable "$1" does not seem to be initialized}

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

{$WARN 5058 off : Variable "$1" does not seem to be initialized}
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
{$WARN 5058 on : Variable "$1" does not seem to be initialized}

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

{$WARN 5058 off : Variable "$1" does not seem to be initialized}
procedure UA_BrowseResponse_init(out p: UA_BrowseResponse);
begin
  FillChar(p, SizeOf(p), #0);
end;
{$WARN 5058 on : Variable "$1" does not seem to be initialized}

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

{$WARN 5058 off : Variable "$1" does not seem to be initialized}
procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
begin
  FillChar(p, SizeOf(p), #0);
end;
{$WARN 5058 on : Variable "$1" does not seem to be initialized}

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
  uas: UA_STRING;
begin
  uas := _UA_STRING_ALLOC(s);
  UA_Variant_setScalarCopy(@v, @uas, @UA_TYPES[UA_TYPES_STRING]);
  UA_String_clear(uas);
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
  result.NodeId:=UA_NODEID_NUMERIC(nsIndex, identifier);
  result.ServerIndex:=0;
  result.NamespaceUri:=UA_STRING_NULL;
end;

procedure UA_init(p: Pointer; const _type: PUA_DataType);
begin
  FillChar(p^, _type^.MemSize, #0);
end;

function UA_Client_connect_username(client: PUA_Client; const endpointUrl, username, password: AnsiString): UA_StatusCode;
begin
  Result := UA_Client_connectUsername(client, endpointUrl, username, password);
end;

(* Connect to the server and create+activate a Session with the given username
 * and password. This first set the UserIdentityToken in the client config and
 * then calls the regular connect method. *)
function UA_Client_connectUsername(client: PUA_Client; const endpointUrl, username, password: AnsiString): UA_StatusCode;
var
  identityToken: PUA_UserNameIdentityToken;
  cc: PUA_ClientConfig;
begin
  identityToken := PUA_UserNameIdentityToken(UA_new(@UA_TYPES[UA_TYPES_USERNAMEIDENTITYTOKEN])); //UA_UserNameIdentityToken_new()
  if identityToken=nil then
    Result:=UA_STATUSCODE_BADOUTOFMEMORY
  else begin
    identityToken^.userName := _UA_STRING_ALLOC(username);
    identityToken^.password := _UA_STRING_ALLOC(password);
    cc := UA_Client_getConfig(client);
    UA_clear(@cc^.userIdentityToken, @UA_TYPES[UA_TYPES_EXTENSIONOBJECT]); //UA_ExtensionObject_clear()
    cc^.userIdentityToken.encoding := UA_EXTENSIONOBJECT_DECODED;
    cc^.userIdentityToken.content.Decoded._Type := @UA_TYPES[UA_TYPES_USERNAMEIDENTITYTOKEN];
    cc^.userIdentityToken.content.Decoded.Data := identityToken;
    Result := UA_Client_connect(client, endpointUrl);
  end;
end;

function UA_Client_Service_read(client: PUA_Client; const request: UA_ReadRequest): UA_ReadResponse;
begin
  __UA_Client_Service(client, @request, @UA_TYPES[UA_TYPES_READREQUEST], @Result, @UA_TYPES[UA_TYPES_READRESPONSE]);
end;

function UA_Client_Service_browse(client: PUA_Client; const request: UA_BrowseRequest): UA_BrowseResponse;
begin
  __UA_Client_Service(client, @request, @UA_TYPES[UA_TYPES_BROWSEREQUEST], @Result, @UA_TYPES[UA_TYPES_BROWSERESPONSE]);
end;

function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: UA_Variant): UA_StatusCode; overload;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE, @outValue, @UA_TYPES[UA_TYPES_VARIANT]);
end;

{$WARN 5058 off : Variable "$1" does not seem to be initialized}
// taken from ua_client_highlevel.c: __UA_Client_readAttribute()
// (use to read subrange of array variable)
function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const indexRange: AnsiString; out outValue: UA_Variant): UA_StatusCode; overload;
var
  item: UA_ReadValueId;
  request: UA_ReadRequest;
  response: UA_ReadResponse;
begin
  FillChar(item, sizeof(UA_ReadValueId), #0);
  item.nodeId := nodeId;
  item.attributeId := ord(UA_ATTRIBUTEID_VALUE);
  item.indexRange := _UA_STRING_ALLOC(indexRange);
  FillChar(request, sizeof(UA_ReadRequest), #0);
  request.nodesToRead := @item;
  request.nodesToReadSize := 1;
  response := UA_Client_Service_read(client, request);
  Result := response.responseHeader.serviceResult;
  if Result = UA_STATUSCODE_GOOD then begin
    if response.resultsSize = 1 then
      Result := response.results[0].Status
    else
      Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
  end;

  if Result = UA_STATUSCODE_GOOD then begin
    (* Set the StatusCode *)
    if response.results^.Flag and 2 <> 0 then
      Result :=  response.results^.Status;

    (* Return early of no value is given *)
    if response.results^.Flag and 1 <> 0 then begin
      (* Copy value into out *)
       outValue := response.results^.Value;
       UA_Variant_init(response.results^.Value);
    end
    else
      Result := UA_STATUSCODE_BADUNEXPECTEDERROR;
  end;

  UA_clear(@response, @UA_TYPES[UA_TYPES_READRESPONSE]);
  UA_String_clear(item.indexRange);
end;
{$WARN 5058 on : Variable "$1" does not seem to be initialized}

function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: Byte): UA_StatusCode;
var
  value: UA_Variant;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE, @value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then begin
    if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_BYTE]) then
      outValue:= PUA_Byte(value.Data)^
    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(value);
end;

function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: Smallint): UA_StatusCode;
var value: UA_Variant;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE, @value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then begin
    if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT16]) then
      outValue:= PUA_Int16(value.Data)^
    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(value);
end;

function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: Longint): UA_StatusCode; overload;
var
  value: UA_Variant;
begin
  UA_Variant_init(value);
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE, @value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then
  begin
    if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_INT32]) then
      outValue:= PUA_Int32(value.Data)^
    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(value);
end;

function UA_Client_readValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValue: AnsiString): UA_StatusCode; overload;
var
  value: UA_Variant;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE, @value, @UA_TYPES[UA_TYPES_VARIANT]);
  if Result = UA_STATUSCODE_GOOD then
  begin
    if UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_STRING]) then
      SetString(outValue, PAnsiChar(PUA_String(value.Data)^.Data), PUA_String(value.Data)^.Length)
    else
      Result := UA_STATUSCODE_BADTYPEMISMATCH;
  end;
  UA_Variant_clear(value);
end;

function UA_Client_readDataTypeAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outDataType: UA_NodeId): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DATATYPE, @outDataType, @UA_TYPES[UA_TYPES_NODEID]);
end;

function UA_Client_readValueRankAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outValueRank: UA_Int32): UA_StatusCode;
begin
  // -2:Any; -1:Scalar; 0:OneOrMoreDimensions; 1:OneDimension
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUERANK, @outValueRank, @UA_TYPES[UA_TYPES_INT32]);
end;

function UA_Client_readBrowseNameAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outBrowseName: UA_QualifiedName): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_BROWSENAME, @outBrowseName, @UA_TYPES[UA_TYPES_QUALIFIEDNAME]);
end;

function UA_Client_readDisplayNameAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outDisplayName: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DISPLAYNAME, @outDisplayName, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;

function UA_Client_readDescriptionAttribute(client: PUA_Client; const nodeId: UA_NodeId; out outDescription: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_readAttribute(client, @nodeId, UA_ATTRIBUTEID_DESCRIPTION, @outDescription, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;

function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newValue: UA_Variant): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUE, @newValue, @UA_TYPES[UA_TYPES_VARIANT]);
end;

function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: Byte): UA_StatusCode;
var
  uav: UA_Variant;
begin
  UA_Variant_setScalar(@uav, @newValue, @UA_TYPES[UA_TYPES_BYTE]);
  Result := UA_Client_writeValueAttribute(client, nodeId, uav);
end;

function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: Smallint): UA_StatusCode;
var
  uav: UA_Variant;
begin
  UA_Variant_setScalar(@uav, @newValue, @UA_TYPES[UA_TYPES_INT16]);
  Result := UA_Client_writeValueAttribute(client, nodeId, uav);
end;

function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: Longint): UA_StatusCode;
var
  uav: UA_Variant;
begin
  UA_Variant_setScalar(@uav, @newValue, @UA_TYPES[UA_TYPES_INT32]);
  Result := UA_Client_writeValueAttribute(client, nodeId, uav);
end;

function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValue: AnsiString): UA_StatusCode;
var
  uav: UA_Variant;
  uas: UA_String;
begin
  uas := _UA_STRING_ALLOC(newValue);
  UA_Variant_setScalar(@uav, @uas, @UA_TYPES[UA_TYPES_STRING]);
  Result := UA_Client_writeValueAttribute(client, nodeId, uav);
  UA_String_clear(uas);
end;

{$WARN 5058 off : Variable "$1" does not seem to be initialized}
function UA_Client_writeValueAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValues: array of AnsiString): UA_StatusCode;
var
  uav: UA_Variant;
  uas: array of UA_String;
  i: integer;
begin
  SetLength(uas, Length(newValues));
  for i:=Low(newValues) to High(newValues) do
    uas[i] := _UA_STRING_ALLOC(newValues[i]);
  UA_Variant_setArray(@uav, @uas[0], Length(uas), @UA_TYPES[UA_TYPES_STRING]);
  Result := UA_Client_writeValueAttribute(client, nodeId, uav);
  for i:=Low(newValues) to High(newValues) do
    UA_String_clear(uas[i]);
end;
{$WARN 5058 on : Variable "$1" does not seem to be initialized}

function UA_Client_writeDescriptionAttribute(client: PUA_Client; const nodeId: UA_NodeId; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} newDescription: UA_LocalizedText): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_DESCRIPTION, @newDescription, @UA_TYPES[UA_TYPES_LOCALIZEDTEXT]);
end;

function UA_Client_writeDataTypeAttribute(client: PUA_Client; const nodeId: UA_NodeId; newDataType: PUA_NodeId): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_DATATYPE, newDataType, @UA_TYPES[UA_TYPES_NODEID]);
end;

function UA_Client_writeValueRankAttribute(client: PUA_Client; const nodeId: UA_NodeId; const newValueRank: UA_Int32): UA_StatusCode;
begin
  Result := __UA_Client_writeAttribute(client, @nodeId, UA_ATTRIBUTEID_VALUERANK, @newValueRank, @UA_TYPES[UA_TYPES_INT32]);
end;

{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
function UA_CreateSubscriptionRequest_default(): UA_CreateSubscriptionRequest;
begin
  UA_CreateSubscriptionRequest_init(Result);

  Result.RequestedPublishingInterval := 500.0; // [ms]
  Result.RequestedLifetimeCount := 10000; // the number of PublishingIntervals to wait for a new PublishRequest, before realizing that the client is no longer active
  Result.RequestedMaxKeepAliveCount := 10; // how many intervals may be skipped, before an empty notification is sent
  Result.MaxNotificationsPerPublish := 0; // unlimited
  Result.PublishingEnabled := True;
  Result.Priority := 0;
end;

function UA_MonitoredItemCreateRequest_default(NodeId: UA_NodeId): UA_MonitoredItemCreateRequest;
begin
  UA_MonitoredItemCreateRequest_init(Result);

  Result.ItemToMonitor.nodeId := NodeId;
  Result.ItemToMonitor.attributeId := Ord(UA_ATTRIBUTEID_VALUE);
  Result.MonitoringMode := UA_MONITORINGMODE_REPORTING;
  Result.RequestedParameters.SamplingInterval := 250;
  Result.RequestedParameters.DiscardOldest := true;
  Result.RequestedParameters.QueueSize := 1;
end;
{$ENDIF}

{$IFDEF ENABLE_SERVER}
(* Creates a server config on the default port 4840 with no server
 * certificate. *)
function UA_ServerConfig_setDefault(config: PUA_ServerConfig): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimal(config, 4840, nil);
end;

(* Creates a new server config with one endpoint.
 *
 * The config will set the tcp network layer to the given port and adds a single
 * endpoint with the security policy ``SecurityPolicy#None`` to the server. A
 * server certificate may be supplied but is optional. *)
function UA_ServerConfig_setMinimal(config: PUA_ServerConfig; portNumber: UA_UInt16; const certificate: PUA_ByteString): UA_StatusCode;
begin
  Result := UA_ServerConfig_setMinimalCustomBuffer(config, portNumber, certificate, 0, 0);
end;

function UA_Server_addVariableNode(server: PUA_Server; const requestedNewNodeId: UA_NodeId;
                          const parentNodeId: UA_NodeId;
                          const referenceTypeId: UA_NodeId;
                          const browseName: UA_QualifiedName;
                          const typeDefinition: UA_NodeId;
                          const attr: UA_VariableAttributes;
                          nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := __UA_Server_addNode(server, UA_NODECLASS_VARIABLE, @requestedNewNodeId,
                               @parentNodeId, @referenceTypeId, browseName,
                               @typeDefinition, PUA_NodeAttributes(@attr),
                               @UA_TYPES[UA_TYPES_VARIABLEATTRIBUTES],
                               nodeContext, outNewNodeId);
end;

function UA_Server_addObjectTypeNode(server:PUA_Server; const requestedNewNodeId:UA_NodeId;
                            const parentNodeId:UA_NodeId;
                            const referenceTypeId:UA_NodeId;
                            const browseName: UA_QualifiedName;
                            const attr:UA_ObjectTypeAttributes;
                            nodeContext:Pointer; outNewNodeId: PUA_NodeId):UA_StatusCode;
begin
    result:= __UA_Server_addNode(server, UA_NODECLASS_OBJECTTYPE, @requestedNewNodeId,
                               @parentNodeId, @referenceTypeId, browseName,
                               @UA_NODEID_NULL, @attr,
                               @UA_TYPES[UA_TYPES_OBJECTTYPEATTRIBUTES],
                               nodeContext, outNewNodeId);
end;


function UA_Server_addObjectNode(server:PUA_Server; const requestedNewNodeId:UA_NodeId;
                        const parentNodeId:UA_NodeId;
                        const referenceTypeId:UA_NodeId;
                        const browseName: UA_QualifiedName;
                        const typeDefinition: UA_NodeId;
                        const attr:UA_ObjectAttributes;
                        nodeContext:Pointer; outNewNodeId: PUA_NodeId):UA_StatusCode;
begin
    result:= __UA_Server_addNode(server, UA_NODECLASS_OBJECT, @requestedNewNodeId,
                               @parentNodeId, @referenceTypeId, browseName,
                               @typeDefinition, @attr,
                               @UA_TYPES[UA_TYPES_OBJECTATTRIBUTES],
                               nodeContext, outNewNodeId);
end;

function UA_Server_addVariableTypeNode(server:PUA_Server;
                              const requestedNewNodeId:UA_NodeId;
                              const parentNodeId:UA_NodeId;
                              const referenceTypeId:UA_NodeId;
                              const browseName:UA_QualifiedName;
                              const typeDefinition:UA_NodeId;
                              const attr:UA_VariableTypeAttributes;
                              nodeContext:Pointer; outNewNodeId:PUA_NodeId):UA_StatusCode;
begin
  Result:= __UA_Server_addNode(server, UA_NODECLASS_VARIABLETYPE,
    @requestedNewNodeId, @parentNodeId, @referenceTypeId,
    browseName, @typeDefinition,
    @attr,
    @UA_TYPES[UA_TYPES_VARIABLETYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addViewNode(server: PUA_Server; const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName;
  const attr: UA_ViewAttributes;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result:= __UA_Server_addNode(server, UA_NODECLASS_VIEW, @requestedNewNodeId,
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
  Result:= __UA_Server_addNode(server, UA_NODECLASS_REFERENCETYPE,
    @requestedNewNodeId, @parentNodeId, @referenceTypeId,
    browseName, @UA_NODEID_NULL,
    @attr,
    @UA_TYPES[UA_TYPES_REFERENCETYPEATTRIBUTES],
    nodeContext, outNewNodeId);
end;

function UA_Server_addDataTypeNode(Server: PUA_Server;
  const RequestedNewNodeId: UA_NodeId;
  const ParentNodeId: UA_NodeId;
  const ReferenceTypeId: UA_NodeId;
  const BrowseName: UA_QualifiedName;
  const Attr: UA_DataTypeAttributes;
  NodeContext: Pointer; OutNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result:= __UA_Server_addNode(Server, UA_NODECLASS_DATATYPE, @RequestedNewNodeId,
    @ParentNodeId, @ReferenceTypeId, BrowseName,
    @UA_NODEID_NULL, @Attr,
    @UA_TYPES[UA_TYPES_DATATYPEATTRIBUTES],
    NodeContext, OutNewNodeId);
end;

function UA_Server_writeValue(server: PUA_Server; const nodeId: UA_NodeId;
  const Value: UA_Variant): UA_StatusCode;
begin
  Result := __UA_Server_write(server, @nodeId, UA_ATTRIBUTEID_VALUE,
    @UA_TYPES[UA_TYPES_VARIANT], @Value);
end;

function UA_Server_addMethodNode(Server: PUA_Server; const requestedNewNodeId: UA_NodeId;
  const parentNodeId: UA_NodeId;
  const referenceTypeId: UA_NodeId;
  const browseName: UA_QualifiedName ;
  const attr: UA_MethodAttributes;
  method: UA_MethodCallback;
  inputArgumentsSize: SIZE_T;
  const inputArguments: PUA_Argument;
  outputArgumentsSize: SIZE_T;
  const outputArguments: PUA_Argument;
  nodeContext: Pointer; outNewNodeId: PUA_NodeId): UA_StatusCode;
begin
  Result := UA_Server_addMethodNodeEx(Server, requestedNewNodeId, parentNodeId,
    referenceTypeId, browseName, attr, method,
    inputArgumentsSize, inputArguments, UA_NODEID_NULL, nil,
    outputArgumentsSize, outputArguments, UA_NODEID_NULL, nil,
    nodeContext, outNewNodeId);
end;
{$ENDIF}


end.

