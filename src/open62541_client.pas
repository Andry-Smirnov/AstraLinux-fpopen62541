{$CODEPAGE UTF8}
// Open62541 Client
// client.h
//
// created 2026.02.12

unit open62541_client;


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


{$IFNDEF LOAD_DYNAMICALLY}
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
function UA_ClientConfig_setDefaultEncryption(config: PUA_ClientConfig;
  localCertificate, privateKey: UA_ByteString; trustList: PUA_ByteString;
  trustListSize: size_t; revocationList: PUA_ByteString;
  revocationListSize: size_t): UA_StatusCode; cdecl; external libopen62541;
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
{$ENDIF}


function UA_Client_connect_username(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode; deprecated;
function UA_Client_connectUsername(client: PUA_Client;
  const endpointUrl, username, password: AnsiString): UA_StatusCode;
function UA_Client_Service_read(client: PUA_Client;
  const request: UA_ReadRequest): UA_ReadResponse;
function UA_Client_Service_browse(client: PUA_Client;
  const request: UA_BrowseRequest): UA_BrowseResponse;


implementation


end.

