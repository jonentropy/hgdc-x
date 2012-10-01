; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{8CD9F65F-46EC-4E1E-925C-980B347DF657}
AppName=Hgdc-X
AppVersion=0.5.5
AppVerName=Hgdc-X 0.5.5
AppPublisher=Tristan Linnell
AppPublisherURL=https://github.com/tristan2468/hgdc-x
AppSupportURL=https://github.com/tristan2468/hgdc-x
AppUpdatesURL=https://github.com/tristan2468/hgdc-x
DefaultDirName={pf}\Hgdc-X
DefaultGroupName=hgdc-X
DisableProgramGroupPage=yes
LicenseFile=..\..\..\hgdc-x\COPYING
OutputBaseFilename=hgdc-x_setup
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
Source: "..\..\build\release\i386-win32-win32\hgdcx.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\build\release\i386-win32-win32\libeay32.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\build\release\i386-win32-win32\libssl32.dll"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Hgdc-X"; Filename: "{app}\hgdcx.exe"
Name: "{commondesktop}\Hgdc-X"; Filename: "{app}\hgdcx.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\hgdc-x"; Filename: "{app}\hgdcx.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\hgdcx.exe"; Description: "{cm:LaunchProgram,Hgdc-X}"; Flags: nowait postinstall skipifsilent
