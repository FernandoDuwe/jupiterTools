unit DarkModeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, Forms, Controls, Graphics, Windows, DwmApi, registry;

type
  TPreferredAppMode = (
    pamDefault,
    pamAllowDark,
    pamForceDark,
    pamForceLight,
    pamMax
  );

const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20; // Windows 10 1809+
  DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19; // Windows 10 < 20H1

function IsWindowsInDarkMode: Boolean;
procedure ApplyDarkModeToForm(AForm: TForm);
procedure SetTitleBarDarkMode(AForm: TForm; Enable: Boolean);

implementation

function IsWindowsInDarkMode: Boolean;
const
  KEY_PATH = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEY_NAME = 'AppsUseLightTheme';
var
  Registry: TRegistry;
  LightKey: Boolean;
begin
  Result := False;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(KEY_PATH) then
    begin
      if Registry.ValueExists(KEY_NAME) then
        LightKey := Registry.ReadBool(KEY_NAME)
      else
        LightKey := True; // Default to light if key not found
    end
    else
      LightKey := True; // Default to light if key not found
    Result := not LightKey;
  finally
    Registry.Free;
  end;
end;

procedure ApplyDarkModeToControl(AControl: TControl);
var
  I : Integer;
begin
  if AControl is TForm then
  begin
    TForm(AControl).Color := clBlack;
    TForm(AControl).Font.Color := clWhite;
  end
  else if AControl is TWinControl then
  begin
    TWinControl(AControl).Color := clBlack;
    TWinControl(AControl).Font.Color := clWhite;
  end;

  // Recursively apply to child controls
  if AControl is TWinControl then
  begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      ApplyDarkModeToControl(TWinControl(AControl).Controls[I]);
    end;
  end;
end;

procedure ApplyDarkModeToForm(AForm: TForm);
begin
  ApplyDarkModeToControl(AForm);
end;

procedure SetTitleBarDarkMode(AForm: TForm; Enable: Boolean);
var
  Value: Integer;
  DwmSetWindowAttributeProc: function(hWnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  User32Lib: HMODULE;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 10) then // Windows 10 or higher
  begin
    User32Lib := LoadLibrary('user32.dll');
    if User32Lib <> 0 then
    begin
//      DwmSetWindowAttributeProc := @(GetProcAddress(GetModuleHandle('dwmapi.dll'), 'DwmSetWindowAttribute'));
      if Assigned(DwmSetWindowAttributeProc) then
      begin
        Value := Ord(Enable);
        // Try DWMWA_USE_IMMERSIVE_DARK_MODE first (Windows 10 1809+)
        if DwmSetWindowAttributeProc(AForm.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE, @Value, SizeOf(Value)) <> S_OK then
        begin
          // Fallback to DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 for older Windows 10 versions
          DwmSetWindowAttributeProc(AForm.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1, @Value, SizeOf(Value));
        end;
      end;
      FreeLibrary(User32Lib);
    end;
  end;
end;

end.
