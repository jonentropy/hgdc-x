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
// HGDConsts.pas - HGD Client constants taken from net.h
// Uses the synapse networking library

unit HGDConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function GetHGDCErrorMessage(ServerResp: string): string;

const

  HGD_RESP_E_INT: string = 'E_INT';                // Internal error
  HGD_RESP_E_DENY: string =  'E_DENY';             // Access denied
  HGD_RESP_E_FLSIZE: string = 'E_FLSIZE';          // File size invalid
  HGD_RESP_E_FLOOD: string = 'E_FLOOD';            // Flood protect triggered
  HGD_RESP_E_NOPLAY: string = 'E_NOPLAY';          // No track is playing
  HGD_RESP_E_WRTRK: string = 'E_WRTRK';            // Wrong track
  HGD_RESP_E_DUPVOTE: string = 'E_DUPVOTE';        // Duplicate vote
  HGD_RESP_E_SSLAGN: string = 'E_SSLAGN';          // Duplicate SSL negotiation
  HGD_RESP_E_SSLNOAVAIL: string = 'E_SSLNOAVAIL';  // SSL not available
  HGD_RESP_E_INVCMD: string = 'E_INVCMD';          // Invalid command
  HGD_RESP_E_SSLREQ: string = 'E_SSLREQ';          // SSL required
  HGD_RESP_E_SHTDWN: string = 'E_SHTDWN';          // Server is going down
  HGD_RESP_E_KICK: string = 'E_KICK';              // Client misbehaving
  HGD_RESP_E_PERMNOCHG: string = 'E_PERMNOCHG';    // Perms did not change
  HGD_RESP_E_USREXIST: string = 'E_USREXIST';      // User already exists
  HGD_RESP_E_USRNOEXIST: string = 'E_USRNOEXIST';  // User doesn't exist

implementation

function GetHGDCErrorMessage(ServerResp: string): string;
begin
  if ServerResp = HGD_RESP_E_INT then Result := 'Internal error.'
  else if ServerResp = HGD_RESP_E_DENY then Result := 'Access denied.'
  else if ServerResp = HGD_RESP_E_FLSIZE then Result := 'File size is invalid.'
  else if ServerResp = HGD_RESP_E_FLOOD then Result := 'Slow down, d00d!'
  else if ServerResp = HGD_RESP_E_NOPLAY then Result := 'Nothing is playing.'
  else if ServerResp = HGD_RESP_E_WRTRK then Result := 'Wrong track.'
  else if ServerResp = HGD_RESP_E_DUPVOTE then Result :=
    'You have already voted.'

  else if ServerResp = HGD_RESP_E_SSLAGN then Result :=
    'Duplicate SSL Negotiation.'

  else if ServerResp = HGD_RESP_E_SSLNOAVAIL then Result :=
    'SSL in not available.'

  else if ServerResp = HGD_RESP_E_INVCMD then Result := 'Invalid command.'
  else if ServerResp = HGD_RESP_E_SSLREQ then result := 'SSL is required.'
  else if ServerResp = HGD_RESP_E_SHTDWN then result :=
    'Server is shutting down.'

  else if ServerResp = HGD_RESP_E_KICK then Result := 'Stop misbehaving, d00d!'
  else if ServerResp = HGD_RESP_E_PERMNOCHG then Result :=
    'Permissions have already been set.'

  else if ServerResp = HGD_RESP_E_USREXIST then Result :=
    'Username already exists.'

  else if ServerResp = HGD_RESP_E_USRNOEXIST then Result :=
    'Username does not exist.'

  else
    Result := 'Unknown Error Code';
end;

end.

