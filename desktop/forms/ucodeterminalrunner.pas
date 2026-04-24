unit uCodeTerminalRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uJupiterForm,
  JupiterConsts, Process, AsyncProcess, uJupiterAction, SynEdit,
  SynHighlighterBat, SynCompletion;

type

  { TFCodeTerminalRunner }

  TFCodeTerminalRunner = class(TFJupiterForm)
    seData: TSynEdit;
    SynBatSyn1: TSynBatSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProcess: TAsyncProcess;

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;

    procedure Internal_OnReadData(Sender: TObject);
    procedure Internal_OnTerminate(Sender: TObject);

    procedure Internal_OnCancelar(Sender: TObject);
  public
    procedure ExecuteCommand(prCommand : String);
  end;

var
  FCodeTerminalRunner: TFCodeTerminalRunner;

implementation

{$R *.lfm}

{ TFCodeTerminalRunner }

procedure TFCodeTerminalRunner.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FProcess := TAsyncProcess.Create(Self);

  Self.FProcess.Options := [poUsePipes, poStderrToOutPut];
  Self.FProcess.ShowWindow := swoHide;

  Self.FProcess.OnReadData := @Internal_OnReadData;
  Self.FProcess.OnTerminate := @Internal_OnTerminate;

  seData.Lines.Clear;
end;

procedure TFCodeTerminalRunner.FormDestroy(Sender: TObject);
begin
  if Self.FProcess.Running then
    Self.FProcess.Terminate(0);

  Self.FProcess.Free;

  inherited;
end;

procedure TFCodeTerminalRunner.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Cancelar', 'Clique aqui para parar o processamento', ICON_CANCEL, @Internal_OnCancelar));
end;

procedure TFCodeTerminalRunner.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.ActionGroup.Count > 0 then
    if Self.FProcess.Running then
      Self.ActionGroup.GetActionAtIndex(0).Enable
    else
      Self.ActionGroup.GetActionAtIndex(0).Disable;
end;

procedure TFCodeTerminalRunner.Internal_OnReadData(Sender: TObject);
var
  Buffer: array[0..2048] of byte;
  BytesRead: LongInt;
  OutputStr: String;
begin
  while Self.FProcess.Output.NumBytesAvailable > 0 do
  begin
    BytesRead := Self.FProcess.Output.Read(Buffer, SizeOf(Buffer));
    SetString(OutputStr, PChar(@Buffer[0]), BytesRead);

    seData.Lines.Add(OutputStr);
  end;
end;

procedure TFCodeTerminalRunner.Internal_OnTerminate(Sender: TObject);
begin
  Self.Hint := 'Finalizado';

  seData.Lines.Add('Processo finalizado');
end;

procedure TFCodeTerminalRunner.Internal_OnCancelar(Sender: TObject);
begin
  if Self.FProcess.Running then
    Self.FProcess.Terminate(0);
end;

procedure TFCodeTerminalRunner.ExecuteCommand(prCommand: String);
begin
  seData.Lines.Clear;
  seData.Lines.Add('> ' + prCommand);
  seData.Lines.Add(EmptyStr);

  Self.Caption := prCommand;

  Self.Hint := 'Executando: ' + prCommand;

  Self.FProcess.Executable := 'cmd.exe';
  Self.FProcess.Parameters.Clear;
  Self.FProcess.Parameters.Add('/C');
  Self.FProcess.Parameters.Add(prCommand);

  Self.FProcess.Execute;
end;

end.

