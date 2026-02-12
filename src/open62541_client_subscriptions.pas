{$CODEPAGE UTF8}
// Open62541 Client Subscriptions
// client_subscriptions.h
//
// created 2026.02.12

unit open62541_client_subscriptions;


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
  open62541_types,
  open62541_types_generated
  ;


{$IFDEF UA_ENABLE_SUBSCRIPTIONS}
type
  // Callbacks defined for Subscriptions
  UA_Client_DeleteSubscriptionCallback = procedure(
    client: PUA_Client;
    subId: UA_UInt32;
    subContext: Pointer); cdecl;
  UA_Client_StatusChangeNotificationCallback = procedure(
    client: PUA_Client;
    subId: UA_UInt32;
    subContext: Pointer;
    notification: PUA_StatusChangeNotification); cdecl;
  // Callback for the deletion of a MonitoredItem
  UA_Client_DeleteMonitoredItemCallback = procedure(
    client: PUA_Client;
    subId: UA_UInt32;
    subContext: Pointer;
    monId: UA_UInt32;
    monContext: Pointer); cdecl;
  // Callback for DataChange notifications
  UA_Client_DataChangeNotificationCallback = procedure(
    client: PUA_Client;
    subId: UA_UInt32;
    subContext: Pointer;
    monId: UA_UInt32;
    monContext: Pointer;
    value: PUA_DataValue); cdecl;
  // Callback for Event notifications
  UA_Client_EventNotificationCallback = procedure(
    client: PUA_Client;
    subId: UA_UInt32;
    subContext: Pointer;
    monId: UA_UInt32;
    monContext: Pointer;
    nEventFields: size_t;
    eventFields: PUA_Variant); cdecl;
{$ENDIF}


implementation


end.

