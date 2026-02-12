{$CODEPAGE UTF8}
// Open62541 Network
// network.h
//
// created 2026.02.12

unit open62541_network;


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
  open62541_common,
  open62541_server
  ;


type
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
    localMaxMessageSize: UA_UInt32;   // (0 = unbounded)
    remoteMaxMessageSize: UA_UInt32;  // (0 = unbounded)
    localMaxChunkCount: UA_UInt32;    // (0 = unbounded)
    remoteMaxChunkCount: UA_UInt32;   // (0 = unbounded)
  end;

  UA_ConnectionState = (UA_CONNECTION_CLOSED, UA_CONNECTION_OPENING, UA_CONNECTION_ESTABLISHED);

  UA_SecureChannel = record {undefined structure} end;

  UA_SOCKET = Integer;

  PUA_Connection  = ^UA_Connection;
  UA_Connection = record
    state: UA_ConnectionState;
    config: UA_ConnectionConfig;
    channel: ^UA_SecureChannel;
    sockfd: UA_SOCKET;
    openingDate: UA_DateTime;
    handle: Pointer;
    incompleteChunk: UA_ByteString;
    connectCallbackID: UA_UInt64;
    getSendBuffer: function (connection: PUA_Connection; length: size_t; buf: PUA_ByteString): UA_StatusCode; cdecl;
    releaseSendBuffer: procedure (connection: PUA_Connection; buf: PUA_ByteString); cdecl;
    send: function (connection: PUA_Connection; buf: PUA_ByteString): UA_StatusCode; cdecl;
    recv: function (connection: PUA_Connection; response: PUA_ByteString; timeout: UA_UInt32): UA_StatusCode; cdecl;
    releaseRecvBuffer: procedure (connection: PUA_Connection; buf: PUA_ByteString); cdecl;
    close: procedure (connection: PUA_Connection); cdecl;
    free: procedure (connection: PUA_Connection); cdecl;
  end;

  UA_ConnectClientConnection = function(
    config: UA_ConnectionConfig; endpointUrl: UA_String;
    timeout: UA_UInt32; logger: PUA_Logger): UA_Connection; cdecl;

{$IFDEF ENABLE_SERVER}
  PUA_ServerNetworkLayer = ^UA_ServerNetworkLayer;

  UA_ServerNetworkLayer = record
    // Internal data
    handle: Pointer;
    // Points to external memory, i.e. handled by server or client
    statistics: PUA_NetworkStatistics;
    discoveryUrl: UA_String;
    localConnectionConfig: UA_ConnectionConfig;
    // Start listening on the networklayer.
    // @param nl The network layer
    // @return Returns UA_STATUSCODE_GOOD or an error code.
    start: function(nl: PUA_ServerNetworkLayer; const logger: PUA_Logger; const customHostname: PUA_String): UA_StatusCode; cdecl;
    // Listen for new and closed connections and arriving packets. Calls
    // UA_Server_processBinaryMessage for the arriving packets. Closed
    // connections are picked up here and forwarded to
    // UA_Server_removeConnection where they are cleaned up and freed.
    // @param nl The network layer
    // @param server The server for processing the incoming packets and for closing connections.
    // @param timeout The timeout during which an event must arrive in milliseconds
    // @return A statuscode for the status of the network layer.
    listen: function(nl: PUA_ServerNetworkLayer; server: PUA_Server; timeout: UA_UInt16): UA_StatusCode; cdecl;
    // Close the network socket and all open connections. Afterwards, the
    // network layer can be safely deleted.
    //
    // @param nl The network layer
    // @param server The server that processes the incoming packets and for closing connections before deleting them.
    // @return A statuscode for the status of the closing operation.
    stop: procedure(nl: PUA_ServerNetworkLayer; server: PUA_Server); cdecl;
    // Deletes the network layer context. Call only after stopping.
    clear: procedure(nl: PUA_ServerNetworkLayer); cdecl;
  end;
{$ENDIF}


implementation


end.

