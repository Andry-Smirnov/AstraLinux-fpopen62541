{$CODEPAGE UTF8}
// Open62541 PKI
// plugin/pki.h
//
// created 2026.02.12

unit open62541_pki;


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
  open62541_types_h
  ;


type
  PUA_CertificateVerification  = ^UA_CertificateVerification;
  UA_CertificateVerification = record
    context: Pointer;
    verifyCertificate: function (verificationContext: Pointer;
      certificate: PUA_ByteString): UA_StatusCode; cdecl;
    verifyApplicationURI: function (verificationContext: Pointer;
      certificate: PUA_ByteString; applicationURI: PUA_String): UA_StatusCode; cdecl;
    deleteMembers: procedure (cv:PUA_CertificateVerification); cdecl;
  end;


implementation


end.

