{$CODEPAGE UTF8}
// Open62541 Security Policy
// plugin/securitypolicy.h
//
// created 2026.02.12

unit open62541_securitypolicy;


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
  open62541_types
  ;


type
  PUA_SecurityPolicy = ^UA_SecurityPolicy;
  UA_SecurityPolicy = record
      // Additional data
      policyContext: Pointer;

      // The policy uri that identifies the implemented algorithms
      policyUri: UA_ByteString;

      // The local certificate is specific for each SecurityPolicy since it
      // depends on the used key length.
      localCertificate: UA_ByteString;

      // Function pointers grouped into modules
{ TODO:
      UA_SecurityPolicyAsymmetricModule asymmetricModule;
      UA_SecurityPolicySymmetricModule symmetricModule;
      UA_SecurityPolicySignatureAlgorithm certificateSigningAlgorithm;
      UA_SecurityPolicyChannelModule channelModule;
      UA_CertificateVerification *certificateVerification;
}
      logger: PUA_Logger;

      // Updates the ApplicationInstanceCertificate and the corresponding private
      // key at runtime.
      updateCertificateAndPrivateKey: function(
        policy: PUA_SecurityPolicy;
        const newCertificate: UA_ByteString;
        const newPrivateKey: UA_ByteString): UA_StatusCode;

      // Deletes the dynamic content of the policy
      deleteMembers: procedure(policy: PUA_SecurityPolicy);
  end;


{$IFNDEF LOAD_DYNAMICALLY}
function UA_SecurityPolicy_None(policy: PUA_SecurityPolicy; certificateVerification: PUA_CertificateVerification; const localCertificate: UA_ByteString; const logger: PUA_Logger): UA_StatusCode; cdecl; external libopen62541;
{$ENDIF}

implementation


end.

