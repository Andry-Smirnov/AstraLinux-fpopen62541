{$CODEPAGE UTF8}
// Open62541 Constants
//
// created 2026.02.11

unit open62541_const;


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


const
{$IFDEF UNIX}
  {$IFDEF UA_VER1_5}
  libopen62541 = 'libopen62541.so.1.5';
  {$ENDIF}
  {$IFDEF UA_VER1_4}
  libopen62541 = 'libopen62541.so.1.4';
  {$ENDIF}
  {$IFDEF UA_VER1_3}
  libopen62541 = 'libopen62541.so.1.3';
  {$ENDIF}
  {$IFDEF UA_VER1_2}
  libopen62541 = 'libopen62541.so.1.2';
  {$ENDIF}
  //libopen62541 = 'libopen62541.so';
{$ELSE}
  // GCC libgcc_s_sjlj-1.dll and libwinpthread-1.dll are also required
  //  they can be downloaded from packages at http://win-builds.org/1.5.0/packages/windows_32/
  {$IFDEF UA_VER1_5}
  libopen62541 = 'libopen62541.1.5.dll';
  {$ENDIF}
  {$IFDEF UA_VER1_4}
  libopen62541 = 'libopen62541.1.4.dll';
  {$ENDIF}
  {$IFDEF UA_VER1_3}
  libopen62541 = 'libopen62541.1.3.dll';
  {$ENDIF}
  {$IFDEF UA_VER1_2}
  libopen62541 = 'libopen62541.1.2.dll';
  {$ENDIF}
  //libopen62541 = 'libopen62541.dll';
{$ENDIF}


implementation


end.
