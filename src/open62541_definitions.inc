{$DEFINE LOAD_DYNAMICALLY}

{$IFDEF FPC}
  {$IFDEF LOAD_DYNAMICALLY}
    {$MODE DELPHI}
    { $HINT DELPHI MODE SELECTED}
  {$ENDIF}
  {$PACKENUM 4} // GCC on x86 enums have size of 4 bytes
  {$PACKRECORDS C}
{$ELSE}
  {$MINENUMSIZE 4}
  {$ALIGN 4}
{$ENDIF}

{$POINTERMATH ON}

// -----------------------------------------------------------------------------

// Use open62541 v1.5
{ $DEFINE UA_VER1_5}
// Use open62541 v1.4
{ $DEFINE UA_VER1_4}
// Use open62541 v1.3
{ $DEFINE UA_VER1_3}
{$IFNDEF UA_VER1_5}
  {$IFNDEF UA_VER1_4}
    {$IFNDEF UA_VER1_3}
      {$DEFINE UA_VER1_2}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$DEFINE ENABLE_SERVER}

// -----------------------------------------------------------------------------

// --- config.h ---
{$I open62541_config.h.inc}
