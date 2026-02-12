{$CODEPAGE UTF8}
// Open62541 Server Сonfig
// server_config.h
//
// created 2026.02.12

unit open62541_server_config;

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
  open62541_types_generated,
  open62541_common,
  open62541_network
  ;


{$IFDEF ENABLE_SERVER}
type
  UA_ServerConfig = record
    logger: UA_Logger;
    // Server Description:
    // The description must be internally consistent.
    // - The ApplicationUri set in the ApplicationDescription must match the
    //   URI set in the server certificate
    buildInfo: UA_BuildInfo;
    applicationDescription: UA_ApplicationDescription;
    serverCertificate: UA_ByteString;

    // Delay in ms from the shutdown signal (ctrl-c) until the actual shutdown.
    // Clients need to be able to get a notification ahead of time.
    shutdownDelay: UA_Double;

    // Rule Handling

    // Verify that the server sends a timestamp in the request header
    verifyRequestTimestamp: UA_RuleHandling;
    // Variables (that don't have a DataType of BaseDataType) must not have
    // an empty variant value. The default behaviour is to auto-create
    // a matching zeroed-out value for empty VariableNodes when they are added
    allowEmptyVariables: UA_RuleHandling;

    // Custom DataTypes. Attention! Custom datatypes are not cleaned up together
    // with the configuration. So it is possible to allocate them on ROM
    customDataTypes:PUA_DataTypeArray;

    // NOTE: See the section on :ref:`generic-types`. Examples for working
    //   with custom data types are provided in /examples/custom_datatype/

    // Networking
    networkLayersSize: size_t;
    networkLayers: PUA_ServerNetworkLayer;
    customHostname: UA_String;

  // FIXME: define the remaining fields
  end;
  PUA_ServerConfig = ^UA_ServerConfig;
{$ENDIF}


implementation


end.

