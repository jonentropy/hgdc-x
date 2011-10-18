{
 * Copyright (c) 2011, Tristan Linnell <tris@canthack.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
}

// hgdc-x Cross Platform hgd client written in Lazarus/Freepascal
// resolver.pas - Patch to FPC's name resolution. Uses libc calls.

unit Resolver;

{$mode objfpc}{$H+}

interface

uses
  initc, Classes, SysUtils, Sockets;

type
  THostEnt = record
    H_Name     : pchar;   { Official name }
    H_Aliases  : ppchar;  { Null-terminated list of aliases}
    H_Addrtype : longint; { Host address type }
    H_length  : longint;  { Length of address }
    H_Addr : ppchar;      { null-terminated list of adresses }
  end;

  PHostEntry = ^THostEnt;
  THostAddr = in_addr;
  PHostAddr = ^THostAddr;

// From libc
function gethostbyname(Name: Pchar): PHostEntry; cdecl; external clib;
function NameLookup(Name: string; out IP: string): boolean;

implementation

function NameLookup(Name: string; out IP: string): boolean;
var
  PHost : PHostEntry;
  Addr: THostAddr;
begin
  IP := Name;
  PHost := GetHostByName(PChar(Name));

  If PHost <> nil then
  begin
    Addr := NetToHost(PHostAddr(PHost^.H_Addr[0])^);
    IP := HostAddrToStr(Addr);
    Result := True;
  end;
end;

end.
