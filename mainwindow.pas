unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Tor, Registry, FileUtil, process, windows;

type
  TUpdateStatusThread = class(TThread)
  public
    Version: String;
  private
    Running: Boolean;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  end;

  TUpdateThread = class(TThread)
  private
    Updated: Boolean;
    procedure Done;
  protected
    procedure Execute; override;
  end;

  TTorThread = class(TThread)
  private
    AProcess: TProcess;
    procedure PrintStdOut;
  protected
    procedure Execute; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    ButtonRunStop: TButton;
    ButtonEditConfig: TButton;
    CheckBoxStartup: TCheckBox;
    CheckBoxUpdate: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelVersion: TLabel;
    LabelUpdate: TLabel;
    LabelStatus: TLabel;
    MemoStdout: TMemo;
    TrayIcon: TTrayIcon;
    procedure ButtonEditConfigClick(Sender: TObject);
    procedure ButtonRunStopClick(Sender: TObject);
    procedure CheckBoxStartupChange(Sender: TObject);
    procedure CheckBoxUpdateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    FirstShow: Boolean;
    UpdateTorThread: TUpdateThread;
    UpdateThread: TUpdateStatusThread;
    TorThread: TTorThread;
    Reg: TRegistry;
    procedure StartTor;
    procedure CheckTorUpdate;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FirstShow:=True;

  FileUtil.CopyFile(ParamStr(0), TorBaseFolder + 'TorController.exe');
  FileUtil.CopyFile(ExtractFilePath(ParamStr(0)) + 'libeay32.dll', TorBaseFolder + 'libeay32.dll');
  FileUtil.CopyFile(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll', TorBaseFolder + 'ssleay32.dll');

  Reg:=TRegistry.Create;
  Reg.RootKey:=HKEY_CURRENT_USER;

  Reg.OpenKey('SOFTWARE\memstream\TorController', True);
  if not Reg.ValueExists('AutoUpdate') then
    Reg.WriteBool('AutoUpdate', False);

  if not Reg.ValueExists('LastUpdate') then
    Reg.WriteString('LastUpdate', 'Never');

  LabelUpdate.Caption:=Reg.ReadString('LastUpdate');

  UpdateTorThread:=TUpdateThread.Create(True);
  TorThread:=TTorThread.Create(True);
  UpdateThread:=TUpdateStatusThread.Create(False);

  if Reg.ReadBool('AutoUpdate') then
  begin
    CheckBoxUpdate.Checked:=True;
    CheckTorUpdate;
  end;

  Reg.CloseKey;
  Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run', True);
  if Reg.ValueExists('TorController') then
  begin
    CheckBoxStartup.Checked:=True;
    StartTor;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FirstShow then
  begin
    FirstShow:=False;
    if CheckBoxStartup.Checked then
      Hide;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    Hide;
  end;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  MainForm.WindowState:=wsNormal;
  ShowWindow(MainForm.Handle, SW_RESTORE);
  Show;
end;

procedure TMainForm.ButtonRunStopClick(Sender: TObject);
begin
  MemoStdout.Clear;
  ButtonRunStop.Enabled:=False;
  if TorRunning then
  begin
    TorStop;
    TorThread.Free;
    UpdateTorThread.Free;
    TorThread:=TTorThread.Create(True);
    UpdateTorThread:=TUpdateThread.Create(True);
    ButtonRunStop.Enabled:=True;
  end
  else
  begin
    StartTor;
  end;
end;

procedure TMainForm.ButtonEditConfigClick(Sender: TObject);
var
  AProcess: TProcess;
begin
  SysUtils.CreateDir(SysUtils.GetEnvironmentVariable('appdata')+'\tor\');
  if not FileExists(SysUtils.GetEnvironmentVariable('appdata')+'\tor\torrc') then
    FileCreate(SysUtils.GetEnvironmentVariable('appdata')+'\tor\torrc');

  AProcess:=TProcess.Create(nil);
  AProcess.Executable:='notepad';
  AProcess.Parameters.Add(SysUtils.GetEnvironmentVariable('appdata')+'\tor\torrc');
  AProcess.Options:=AProcess.Options+[poDetached];
  AProcess.Execute;
end;

procedure TMainForm.CheckBoxStartupChange(Sender: TObject);
begin
  Reg.CloseKey;
  Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run', True);
  if CheckBoxStartup.Checked then
  begin
    Reg.WriteString('TorController', TorBaseFolder + 'TorController.exe');
  end
  else
  begin
    Reg.DeleteValue('TorController');
  end;
end;

procedure TMainForm.CheckBoxUpdateChange(Sender: TObject);
begin
  Reg.CloseKey;
  Reg.OpenKey('SOFTWARE\memstream\TorController', True);
  Reg.WriteBool('AutoUpdate', CheckBoxUpdate.Checked);
end;

procedure TMainForm.StartTor;
begin
  if not FileExists(TorBaseFolder+'\tor\Tor\tor.exe') then
  begin
    ButtonRunStop.Enabled:=False;
    CheckTorUpdate;
  end
  else
  begin
    TorThread.Resume;
  end;
end;

procedure TMainForm.CheckTorUpdate;
begin
  UpdateTorThread.Resume;
end;

procedure TUpdateStatusThread.ShowStatus;
begin
  MainForm.LabelVersion.Caption:=Version;
  MainForm.ButtonEditConfig.Enabled:=not Running;
  if Running then
  begin
    MainForm.ButtonRunStop.Caption:='Stop tor';
    MainForm.LabelStatus.Caption:='Running';
    MainForm.LabelStatus.Font.Color:=RGBToColor(0, 128, 0);
  end
  else
  begin
    MainForm.ButtonRunStop.Caption:='Start tor';
    MainForm.LabelStatus.Caption:='Down';
    MainForm.LabelStatus.Font.Color:=RGBToColor(128, 0, 0);
  end;
end;

procedure TUpdateStatusThread.Execute;
begin
  Version:=TorVersion;
  while not Terminated do
  begin
    Running:=TorRunning;
    Synchronize(@ShowStatus);
    Sleep(1000);
  end;
end;

procedure TUpdateThread.Done;
var
  Reg: TRegistry;
begin
  if Updated then
  begin
    Reg:=TRegistry.Create();
    Reg.RootKey:=HKEY_CURRENT_USER;
    Reg.OpenKey('SOFTWARE\memstream\TorController', True);
    Reg.WriteString('LastUpdate', DateToStr(Now));
    Reg.Free;

    MainForm.LabelUpdate.Caption:=DateToStr(Now);

    if MainForm.TorThread.Suspended then
      MainForm.TorThread.Resume;

    MainForm.UpdateThread.Version:=TorVersion;
  end;
  MainForm.LabelVersion.Caption:=TorVersion;
end;

procedure TUpdateThread.Execute;
var
  CurrVersion: String;
begin
  Updated:=False;
  CurrVersion:=TorCurrentVersion(TorCurrentLink);

  if CurrVersion = '' then
  begin
    ShowMessage('Fail to check new tor version');
    Exit;
  end;

  if (not FileExists(TorBaseFolder + 'tor\Tor\tor.exe')) or (TorVersion <> CurrVersion) then
  begin
    ShowMessage('Updaing...');
    Updated:=True;
    TorUpdate;
  end;

  Synchronize(@Done);
end;

procedure TTorThread.PrintStdOut;
var
  Text: String;
begin
  MainForm.ButtonRunStop.Enabled:=True;
  Text:='';
  while AProcess.Output.NumBytesAvailable > 0 do
  begin
    Text:=Text+Chr(AProcess.Output.ReadByte);
  end;
  MainForm.MemoStdout.Text:=MainForm.MemoStdout.Text+Text;
end;

procedure TTorThread.Execute;
begin
  AProcess:=TProcess.Create(nil);
  AProcess.Executable:=TorBaseFolder+'tor\Tor\tor.exe';
  AProcess.Options:=AProcess.Options+[poUsePipes, poStderrToOutPut];
  AProcess.ShowWindow:=swoHIDE;
  AProcess.Execute;

  while (not Terminated) do
  begin
    if AProcess.Output.NumBytesAvailable > 0 then
      Synchronize(@PrintStdOut);
    if not AProcess.Running then
      break;
    Sleep(250);
  end;
end;

end.

