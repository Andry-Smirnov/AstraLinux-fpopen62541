{$CODEPAGE UTF8}
// Open62541 Client Config
// client_config.h
//
// created 2026.02.12

unit open62541_client_config;


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
  open62541_types_generated,
  open62541_common,
  open62541_network,
  open62541_securitypolicy,
  open62541_pki
  ;


type
  UA_ClientConfig = record
    // Basic client configuration

    // User-defined data attached to the client
    clientContext: Pointer;
    // Logger used by the client
    logger: UA_Logger;
    // Response timeout in ms
    timeout: UA_UInt32;

    // The description must be internally consistent.
    // - The ApplicationUri set in the ApplicationDescription must match the
    //   URI set in the server certificate
    clientDescription: UA_ApplicationDescription;

    // Basic connection configuration

    // Configured User-Identity Token
    userIdentityToken: UA_ExtensionObject;
    // None, Sign, SignAndEncrypt. The default is invalid. This indicates
    // the client to select any matching endpoint
    securityMode: UA_MessageSecurityMode;
    // SecurityPolicy for the SecureChannel. An empty string indicates
    // the client to select any matching SecurityPolicy. *)
    securityPolicyUri: UA_String;

    // Advanced connection configuration

    // If either endpoint or userTokenPolicy has been set (at least one non-zero
    // byte in either structure), then the selected Endpoint and UserTokenPolicy
    // overwrite the settings in the basic connection configuration.
    // The userTokenPolicy array in the EndpointDescription is ignored.
    // The selected userTokenPolicy is set in the dedicated configuration field.
    //
    // If the advanced configuration is not set, the client will write to it the
    // selected Endpoint and UserTokenPolicy during GetEndpoints.
    //
    // The information in the advanced configuration is used during reconnect
    // when the SecureChannel was broken.
    endpoint: UA_EndpointDescription;
    userTokenPolicy: UA_UserTokenPolicy;

    // Advanced client configuration

    // Lifetime in ms (then the channel needs to be renewed)
    secureChannelLifeTime: UA_UInt32;
    // Session timeout in ms
    requestedSessionTimeout: UA_UInt32;
    localConnectionConfig: UA_ConnectionConfig;
    // Connectivity check interval in ms. 0 = background task disabled
    connectivityCheckInterval: UA_UInt32 ;
    // Custom DataTypes. Attention! Custom datatypes are not cleaned
    // up together with the configuration. So it is possible
    // to allocate them on ROM
    customDataTypes: ^UA_DataTypeArray;

    // Available SecurityPolicies
    securityPoliciesSize: size_t;
    securityPolicies: ^UA_SecurityPolicy;

    // Certificate Verification Plugin
    certificateVerification: UA_CertificateVerification;

    // Callbacks for async connection handshakes
    initConnectionFunc: UA_ConnectClientConnection;
    pollConnectionFunc: function(
      client: PUA_Client;
      context: Pointer;
      timeout: UA_UInt32): UA_StatusCode; cdecl;

    // Callback for state changes. The client state is differentated into the
    // SecureChannel state and the Session state. The connectStatus is set if
    // the client connection (including reconnects) has failed and the client
    // has to "give up". If the connectStatus is not set, the client still has
    // hope to connect or recover.
    stateCallback: procedure (
      client: PUA_Client;
      channelState: UA_SecureChannelState;
      sessionState: UA_SessionState;
      connectStatus: UA_StatusCode); cdecl;

    // When connectivityCheckInterval is greater than 0, every
    // connectivityCheckInterval (in ms), a async read request is performed on
    // the server. inactivityCallback is called when the client receive no
    // response for this read request The connection can be closed, this in an
    // attempt to recreate a healthy connection. *)
    inactivityCallback: procedure (client: PUA_Client); cdecl;

    {$IFDEF UA_ENABLE_SUBSCRIPTIONS}
    // Number of PublishResponse queued up in the server
    outStandingPublishRequests: UA_UInt16;

    // If the client does not receive a PublishResponse after the defined delay
    // of ``(sub->publishingInterval * sub->maxKeepAliveCount) +
    // client->config.timeout)``, then subscriptionInactivityCallback is called
    // for the subscription.. *)
    subscriptionInactivityCallback: procedure(
      client: PUA_Client;
      subscriptionId: UA_UInt32;
      subContext: Pointer); cdecl;
    {$ENDIF}
  end;
  PUA_ClientConfig = ^UA_ClientConfig;


implementation


end.

