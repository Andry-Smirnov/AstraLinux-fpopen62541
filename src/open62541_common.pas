{$CODEPAGE UTF8}
// Open62541 Common
// common.h
//
// created 2026.02.12

unit open62541_common;


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


type

  //
  // Standard-Defined Constants
  //
  // This section contains numerical and string constants that are defined in the
  // OPC UA standard.


  // Attribute Id (.. _attribute-id:)
  //
  // Every node in an OPC UA information model contains attributes depending on
  // the node type. Possible attributes are as follows:

  UA_AttributeId = (
    UA_ATTRIBUTEID_NODEID                  = 1,
    UA_ATTRIBUTEID_NODECLASS               = 2,
    UA_ATTRIBUTEID_BROWSENAME              = 3,
    UA_ATTRIBUTEID_DISPLAYNAME             = 4,
    UA_ATTRIBUTEID_DESCRIPTION             = 5,
    UA_ATTRIBUTEID_WRITEMASK               = 6,
    UA_ATTRIBUTEID_USERWRITEMASK           = 7,
    UA_ATTRIBUTEID_ISABSTRACT              = 8,
    UA_ATTRIBUTEID_SYMMETRIC               = 9,
    UA_ATTRIBUTEID_INVERSENAME             = 10,
    UA_ATTRIBUTEID_CONTAINSNOLOOPS         = 11,
    UA_ATTRIBUTEID_EVENTNOTIFIER           = 12,
    UA_ATTRIBUTEID_VALUE                   = 13,
    UA_ATTRIBUTEID_DATATYPE                = 14,
    UA_ATTRIBUTEID_VALUERANK               = 15,
    UA_ATTRIBUTEID_ARRAYDIMENSIONS         = 16,
    UA_ATTRIBUTEID_ACCESSLEVEL             = 17,
    UA_ATTRIBUTEID_USERACCESSLEVEL         = 18,
    UA_ATTRIBUTEID_MINIMUMSAMPLINGINTERVAL = 19,
    UA_ATTRIBUTEID_HISTORIZING             = 20,
    UA_ATTRIBUTEID_EXECUTABLE              = 21,
    UA_ATTRIBUTEID_USEREXECUTABLE          = 22,
    UA_ATTRIBUTEID_DATATYPEDEFINITION      = 23
  );

  // Rule Handling
  //
  // The RuleHanding settings define how error cases that result from rules in the
  // OPC UA specification shall be handled. The rule handling can be softened,
  // e.g. to workaround misbehaving implementations or to mitigate the impact of
  // additional rules that are introduced in later versions of the OPC UA
  // specification.
  UA_RuleHandling = (
    UA_RULEHANDLING_DEFAULT = 0,
    UA_RULEHANDLING_ABORT,  // Abort the operation and return an error code
    UA_RULEHANDLING_WARN,   // Print a message in the logs and continue
    UA_RULEHANDLING_ACCEPT  // Continue and disregard the broken rule
  );


  // Connection State
  UA_SecureChannelState = (
    UA_SECURECHANNELSTATE_CLOSED,
    UA_SECURECHANNELSTATE_HEL_SENT,
    UA_SECURECHANNELSTATE_HEL_RECEIVED,
    UA_SECURECHANNELSTATE_ACK_SENT,
    UA_SECURECHANNELSTATE_ACK_RECEIVED,
    UA_SECURECHANNELSTATE_OPN_SENT,
    UA_SECURECHANNELSTATE_OPEN,
    UA_SECURECHANNELSTATE_CLOSING
  );
  PUA_SecureChannelState = ^UA_SecureChannelState;

  UA_SessionState = (
    UA_SESSIONSTATE_CLOSED,
    UA_SESSIONSTATE_CREATE_REQUESTED,
    UA_SESSIONSTATE_CREATED,
    UA_SESSIONSTATE_ACTIVATE_REQUESTED,
    UA_SESSIONSTATE_ACTIVATED,
    UA_SESSIONSTATE_CLOSING
  );
  PUA_SessionState = ^UA_SessionState;

  // Statistic counters
  //
  // The stack manage statistic counter for the following layers:
  // - Network
  // - Secure channel
  // - Session
  //
  // The session layer counters are matching the counters of the
  // ServerDiagnosticsSummaryDataType that are defined in the OPC UA Part 5
  // specification. Counter of the other layers are not specified by OPC UA but
  // are harmonized with the session layer counters if possible.
  UA_NetworkStatistics = record
    currentConnectionCount: size_t;
    cumulatedConnectionCount: size_t;
    rejectedConnectionCount: size_t;
    connectionTimeoutCount: size_t;
    connectionAbortCount: size_t;
  end;
  PUA_NetworkStatistics = ^UA_NetworkStatistics;

  UA_SecureChannelStatistics = record
    currentChannelCount: size_t;
    cumulatedChannelCount: size_t;
    rejectedChannelCount: size_t;
    channelTimeoutCount: size_t; // only used by servers
    channelAbortCount: size_t;
    channelPurgeCount: size_t;   // only used by servers
  end;
  PUA_SecureChannelStatistics = ^UA_SecureChannelStatistics;

  UA_SessionStatistics = record
    currentSessionCount: size_t;
    cumulatedSessionCount: size_t;
    securityRejectedSessionCount: size_t; // only used by servers
    rejectedSessionCount: size_t;
    sessionTimeoutCount: size_t;          // only used by servers
    sessionAbortCount: size_t;            // only used by servers
  end;
  PUA_SessionStatistics = ^UA_SessionStatistics;


const
  // Access Level Masks
  // The access level to a node is given by the following constants that
  // are ANDed with the overall access level.
  UA_ACCESSLEVELMASK_READ           = $01 shl 0;
  UA_ACCESSLEVELMASK_WRITE          = $01 shl 1;
  UA_ACCESSLEVELMASK_HISTORYREAD    = $01 shl 2;
  UA_ACCESSLEVELMASK_HISTORYWRITE   = $01 shl 3;
  UA_ACCESSLEVELMASK_SEMANTICCHANGE = $01 shl 4;
  UA_ACCESSLEVELMASK_STATUSWRITE    = $01 shl 5;
  UA_ACCESSLEVELMASK_TIMESTAMPWRITE = $01 shl 6;


implementation


end.

