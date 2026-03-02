{$CODEPAGE UTF8}
// Open62541 UA Types
// ua_types.h
//
// created 2026.02.12

unit open62541_ua_types;


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
  open62541_types_h
  ;


const
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


implementation


end.

