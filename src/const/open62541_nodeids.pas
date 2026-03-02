{$CODEPAGE UTF8}
// Open62541 Node Ids
// nodeids.h
//
// created 2026.02.12

unit open62541_nodeids;


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


const
{$IFDEF UA_VER1_5}
  {$I const/nodeids_1_5.inc}
{$ENDIF}
{$IFDEF UA_VER1_4}
  {$I const/nodeids_1_4.inc}
{$ENDIF}
{$IFDEF UA_VER1_3}
  {$I const/nodeids_1_3.inc}
{$ENDIF}
{$IFDEF UA_VER1_2}
  {$I const/nodeids_1_2.inc}
{$ENDIF}


implementation


end.

