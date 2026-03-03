{$CODEPAGE UTF8}
// Open62541 Types Generated Handling
// types_generated_handling.h
//
// created 2026.02.12

unit open62541_types_generated_handling;


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
  open62541_types_h
//  open62541_types_generated
  ;


procedure UA_Variant_init(out p: UA_Variant);
procedure UA_Variant_clear(var p: UA_Variant);
procedure UA_String_clear(var p: UA_String);
procedure UA_NodeId_clear(var p: UA_NodeId);
procedure UA_CreateSubscriptionRequest_init(out p: UA_CreateSubscriptionRequest);
procedure UA_MonitoredItemCreateRequest_init(out p: UA_MonitoredItemCreateRequest);

procedure UA_BrowseRequest_init(out p: UA_BrowseRequest);
function UA_BrowseRequest_new:PUA_BrowseRequest;
function UA_BrowseRequest_copy(const src: UA_BrowseRequest; out dst: UA_BrowseRequest): UA_StatusCode;
procedure UA_BrowseRequest_deleteMembers(var p: UA_BrowseRequest);
procedure UA_BrowseRequest_clear(var p: UA_BrowseRequest);
procedure UA_BrowseRequest_delete(p: PUA_BrowseRequest);

procedure UA_BrowseResponse_init(out p: UA_BrowseResponse);
function UA_BrowseResponse_new: PUA_BrowseResponse;
function UA_BrowseResponse_copy(const src: UA_BrowseResponse; out dst: UA_BrowseResponse): UA_StatusCode;
procedure UA_BrowseResponse_deleteMembers(var p: UA_BrowseResponse);
procedure UA_BrowseResponse_clear(var p: UA_BrowseResponse);
procedure UA_BrowseResponse_delete(p: PUA_BrowseResponse);

procedure UA_BrowseDescription_init(out p: UA_BrowseDescription);
function UA_BrowseDescription_new: PUA_BrowseDescription;
function UA_BrowseDescription_copy(const src: UA_BrowseDescription; out dst: UA_BrowseDescription): UA_StatusCode;
procedure UA_BrowseDescription_deleteMembers(var p: UA_BrowseDescription);
procedure UA_BrowseDescription_clear(var p: UA_BrowseDescription);
procedure UA_BrowseDescription_delete(p: PUA_BrowseDescription);


implementation


uses
  open62541_types
  ;


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


end.

