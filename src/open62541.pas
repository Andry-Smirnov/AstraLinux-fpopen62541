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

// changed 2026.02.12
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
//  open62541_client_config,
//  open62541_common,
//  open62541_network,
//  open62541_nodeids,
//  open62541_pki,
//  open62541_server,
//  open62541_server_config,
//  open62541_server_config_default,
//  open62541_statuscodes,
{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
//  ,
//  open62541_client_subscriptions
{$ENDIF}
{$IFNDEF LOAD_DYNAMICALLY}
  ,
  open62541_client,
  open62541_client_highlevel,
  open62541_securitypolicy,

  open62541_server_config_default,
{$ENDIF}
//  open62541_types,
  open62541_types_h
//  open62541_types_generated,
//  open62541_types_generated_handling,
//  , open62541_ua_types
  ;


{$IFDEF UA_VER1_5}
  {$I types/generated_1_5.inc}
{$ENDIF}
{$IFDEF UA_VER1_4}
  {$I types/generated_1_4.inc}
{$ENDIF}
{$IFDEF UA_VER1_3}
  {$I types/generated_1_3.inc}
{$ENDIF}
{$IFDEF UA_VER1_2}
  {$I types/generated_1_2.inc}
{$ENDIF}


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


const
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


const
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
  UA_TYPES: PUA_DataType;
  UA_VariableAttributes_default: UA_VariableAttributes;
  UA_ObjectAttributes_default: UA_ObjectAttributes;
  UA_ObjectTypeAttributes_default: UA_ObjectTypeAttributes;
  UA_ReferenceTypeAttributes_default: UA_ReferenceTypeAttributes;
  UA_DataTypeAttributes_default: UA_DataTypeAttributes;


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
{$ENDIF}


end.
