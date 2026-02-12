{$CODEPAGE UTF8}
// Open62541 Types
// types.h
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


const
  UA_EMPTY_ARRAY_SENTINEL = Pointer($01);


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

// types.h

  UA_Boolean = ByteBool;
  PUA_Boolean = ^UA_Boolean;

  UA_Byte = Byte;
  PUA_Byte = ^UA_Byte;

  UA_Int16 = SmallInt;
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

  UA_Float = Single;
  PUA_Float = ^UA_Float;

  UA_Double = Double;
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
    locale: UA_String;
    text: UA_String;
  end;
  PUA_LocalizedText = ^UA_LocalizedText;

  // NumericRange
  //
  // NumericRanges are used to indicate subsets of a (multidimensional) array.
  // They no official data type in the OPC UA standard and are transmitted only
  // with a string encoding, such as "1:2,0:3,5". The colon separates min/max
  // index and the comma separates dimensions. A Single value indicates a range
  // with a Single element (min==max). *)
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

  // Variant (.. _variant:)
  //
  // Variants may contain values of any type together with a description of the
  // content. See the section on :ref:`generic-types` on how types are described.
  // The standard mandates that variants contain built-in data types only. If the
  // value is not of a builtin type, it is wrapped into an :ref:`extensionobject`.
  // open62541 hides this wrapping transparently in the encoding layer. If the
  // data type is unknown to the receiver, the variant contains the original
  // ExtensionObject in binary or XML encoding.
  //
  // Variants may contain a scalar value or an array. For details on the handling
  // of arrays, see the section on :ref:`array-handling`. Array variants can have
  // an additional dimensionality (matrix, 3-tensor, ...) defined in an array of
  // dimension lengths. The actual values are kept in an array of dimensions one.
  // For users who work with higher-dimensions arrays directly, keep in mind that
  // dimensions of higher rank are serialized first (the highest rank dimension
  // has stride 1 and elements follow each other directly). Usually it is simplest
  // to interact with higher-dimensional arrays via ``UA_NumericRange``
  // descriptions (see :ref:`array-handling`).
  //
  // To differentiate between scalar / array variants, the following definition is
  // used. ``UA_Variant_isScalar`` provides simplified access to these checks.
  //
  // - ``arrayLength == 0 && data == NULL``: undefined array of length -1
  // - ``arrayLength == 0 && data == UA_EMPTY_ARRAY_SENTINEL``: array of length 0
  // - ``arrayLength == 0 && data > UA_EMPTY_ARRAY_SENTINEL``: scalar value
  // - ``arrayLength > 0``: array of the given length
  //
  // Variants can also be *empty*. Then, the pointer to the type description is ``NULL``.
  UA_VariantStorageType = (
    UA_VARIANT_DATA,         // The data has the same lifecycle as the variant
    UA_VARIANT_DATA_NODELETE // The data is "borrowed" by the variant and shall not be deleted at the end of the variant's lifecycle
  );

  UA_Variant = record
    _type: PUA_DataType;          // The data type description
    storageType: UA_VariantStorageType;
    arrayLength: size_t;
    data: Pointer;                // Points to the scalar or array data
    arrayDimensionsSize: size_t;  // The number of dimensions
    arrayDimensions: ^UA_UInt32;  // The length of each dimension
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
    content: record
      case LongInt of
        0:  ( encoded: record
              // The nodeid of the datatype
              typeId: UA_NodeId;
              // The bytestring of the encoded data
              body: UA_ByteString;
            end
            );
        1:  ( decoded: record
              _type: PUA_DataType;
              data: Pointer;
            end );
      end;
    end;
  PUA_ExtensionObject = ^UA_ExtensionObject;

  // DataValue
  //
  // A data value with an associated status code and timestamps.
  UA_DataValue = record
    value: UA_Variant;
    sourceTimestamp: UA_DateTime;
    serverTimestamp: UA_DateTime;
    sourcePicoseconds: UA_UInt16;
    serverPicoseconds: UA_UInt16;
    status: UA_StatusCode;
    flag: UA_Byte;
  end;
  PUA_DataValue = ^UA_DataValue;

  // DiagnosticInfo - A structure that contains detailed error and diagnostic information
  // associated with a StatusCode.
  PUA_DiagnosticInfo = ^UA_DiagnosticInfo;
  UA_DiagnosticInfo = record
    flag: UA_Boolean;
    symbolicId: UA_Int32;
    namespaceUri: UA_Int32;
    localizedText: UA_Int32;
    locale: UA_Int32;
    additionalInfo: UA_String;
    innerStatusCode: UA_StatusCode;
    innerDiagnosticInfo: PUA_DiagnosticInfo;
  end;

  // Generic Type Handling (.. _generic-types:)
  //
  // All information about a (builtin/structured) data type is stored in a
  // ``UA_DataType``. The array ``UA_TYPES`` contains the description of all
  // standard-defined types. This type description is used for the following
  // generic operations that work on all types:
  //
  // - ``void T_init(T *ptr)``: Initialize the data type. This is synonymous with
  //   zeroing out the memory, i.e. ``memset(ptr, 0, sizeof(T))``.
  // - ``T* T_new()``: Allocate and return the memory for the data type. The
  //   value is already initialized.
  // - ``UA_StatusCode T_copy(const T *src, T *dst)``: Copy the content of the
  //   data type. Returns ``UA_STATUSCODE_GOOD`` or ``UA_STATUSCODE_BADOUTOFMEMORY``.
  // - ``void T_clear(T *ptr)``: Delete the dynamically allocated content
  //   of the data type and perform a ``T_init`` to reset the type.
  // - ``void T_delete(T *ptr)``: Delete the content of the data type and the
  //   memory for the data type itself.
  //
  // Specializations, such as ``UA_Int32_new()`` are derived from the generic
  // type operations as static inline functions.


//TODO: RELEASE 1.4 and 1.5
{$IFDEF UA_VER1_5}
  UA_DataTypeMember = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    // Human-readable member name
    memberName: PAnsiChar;
    {$ENDIF}
    // The member data type description
    memberType: PUA_DataType;
    // How much padding is there before this member element? For arrays this is
    // the padding before the size_t length member.
    // (No padding between size_t and the following ptr.) For unions,
    // the padding includes the size of the switchfield (the offset from
    // the start of the union type).
    padding: 0..63;
    // The member is an array
    isArray: 0..1;
    // The member is an optional field
    isOptional: 0..1;
  end;
{$IFEND}
{$IFDEF UA_VER1_4}
  UA_DataTypeMember = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    // Human-readable member name
    memberName: PAnsiChar;
    {$ENDIF}
    // The member data type description
    memberType: PUA_DataType;
    // How much padding is there before this member element? For arrays this is
    // the padding before the size_t length member.
    // (No padding between size_t and the following ptr.) For unions,
    // the padding includes the size of the switchfield (the offset from
    // the start of the union type).
    padding: 0..63;
    // The member is an array
    isArray: 0..1;
    // The member is an optional field
    isOptional: 0..1;
  end;
{$ENDIF}
{$IFDEF UA_VER1_3}
  UA_DataTypeMember = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    memberName: PAnsiChar;
    {$ENDIF}
    // The member data type description
    memberType: PUA_DataType;
    // How much padding is there before this member element?
    // For arrays this is the
    // padding before the size_t length member.
    // (No padding between size_t and the following ptr.)
    padding: 0..63;
    // The member is an array
    isArray: 0..1;
    // The member is an optional field
    isOptional: 0..1;
(*
    fill: UA_Byte;
    fill1: UA_Byte;
    fill2: UA_Byte;
    {namespaceZero: UA_Boolean:1;}  // The type of the member is defined in namespace zero.
                                    // In this implementation,
                                    // types from custom namespace may contain
                                    // members from the same namespace or
                                    // namespace zero only.
*)
  end;
{$ENDIF}
{$IFDEF UA_VER1_2}
  UA_DataTypeMember = record
    memberTypeIndex: UA_UInt16;   // Index of the member in the array of data types
    padding: UA_Byte;             (* How much padding is there before this
                                    member element? For arrays this is the
                                    padding before the size_t length member.
                                    (No padding between size_t and the
                                    following ptr.) *)
    flag: Byte;
    {namespaceZero: UA_Boolean:1;}  (* The type of the member is defined in
                                    namespace zero. In this implementation,
                                    types from custom namespace may contain
                                    members from the same namespace or
                                    namespace zero only.*)
    {isArray: UA_Boolean:1;}        // The member is an array
    {isOptional: UA_Boolean:1;}     // The member is an optional field
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    memberName: PAnsiChar;
    {$ENDIF}
  end;
{$ENDIF}

  // The DataType "kind" is an internal type classification. It is used to
  // dispatch handling to the correct routines.
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
    UA_DATATYPEKIND_OPTSTRUCT = 28,      // struct with optional fields
    UA_DATATYPEKIND_UNION = 29,
    UA_DATATYPEKIND_BITFIELDCLUSTER = 30 // bitfields + padding
  );

//TODO: RELEASE 1.5
{$IFDEF UA_VER1_3}
  UA_DataType = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    typeName: PAnsiChar;
    {$ENDIF}
    typeId: UA_NodeId;               // The nodeid of the type
    binaryEncodingId: UA_NodeId;     // NodeId of datatype when encoded as binary
    //xmlEncodingId: UA_NodeId;      // NodeId of datatype when encoded as XML
    memSize: UA_UInt16;              // Size of the struct in memory
    typeKind : 0..63;                // Dispatch index for the handling routines
    pointerFree : 0..1;              // The type (and its members) contains no pointers that need to be freed
    overlayable : 0..1;              // The type has the identical memory layout in memory and on the binary stream.
    membersSize : UA_Byte;           // How many members does the type have?
    members: ^UA_DataTypeMember;
  end;
{$ELSE}
//   UA_DataType = bitpacked record
  UA_DataType = record
    typeId: UA_NodeId;               // The nodeid of the type
    binaryEncodingId: UA_NodeId;     // NodeId of datatype when encoded as binary
    memSize: UA_UInt16;              // Size of the struct in memory
    typeIndex: UA_UInt16;            // Index of the type in the datatypetable
//    typeKind : 0..63;               // Dispatch index for the handling routines
//    pointerFree : 0..1;             // The type (and its members) contains no pointers that need to be freed
    {UA_UInt32 typeKind        : 6;} // Dispatch index for the handling routines
    {UA_UInt32 pointerFree     : 1;} // The type (and its members) contains no pointers that need to be freed
    flags: UA_Int32;
//    overlayable : 0..1;              // The type has the identical memory layout in memory and on the binary stream.
    {UA_UInt32 overlayable     : 1;} // The type has the identical memory layout in memory and on the binary stream.
//    membersSize : UA_Byte;           // How many members does the type have?
    {UA_UInt32 membersSize     : 8;} // How many members does the type have?
//    UA_UInt16  xmlEncodingId;      // NodeId of datatype when encoded as XML
    members: ^UA_DataTypeMember;
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    typeName: PAnsiChar;
    {$ENDIF}
  end;
{$ENDIF}

  // Datatype arrays with custom type definitions can be added in a linked list to
  // the client or server configuration. Datatype members can point to types in
  // the same array via the ``memberTypeIndex``. If ``namespaceZero`` is set to
  // True, the member datatype is looked up in the array of builtin datatypes
  // instead.
  PUA_DataTypeArray = ^UA_DataTypeArray;
  UA_DataTypeArray = record
    next: PUA_DataTypeArray;
    typesSize: size_t;
    types: ^UA_DataType;
  end;


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


implementation


end.

