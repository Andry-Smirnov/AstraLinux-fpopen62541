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
  open62541_types_h,
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


{$IFNDEF LOAD_DYNAMICALLY}
  {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
var
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
{$ENDIF}


implementation


end.

