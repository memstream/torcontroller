unit Tor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, opensslsockets, fphttpclient, SAX_HTML, DOM_HTML, DOM,
  windows, JwaTlHelp32, Zipper, Process;

function TorBaseFolder: String;
function TorCurrentLink: String;
function TorCurrentVersion(CurrentLink: String): String;
function TorGetProcessEntry(out PEOut: TProcessEntry32): boolean;
function TorRunning: Boolean;
function TorVersion: String;
procedure TorStop;
procedure TorUpdate;

implementation

function TorBaseFolder: String;
var
  AppData: String;
begin
  AppData:=SysUtils.GetEnvironmentVariable('appdata');
  if not DirectoryExists(AppData + '\TorController') then
    CreateDir(AppData + '\TorController');
  if not DirectoryExists(AppData + '\TorController\tor') then
    CreateDir(AppData + '\TorController\tor');
  Result:=AppData + '\TorController\';
end;

function TorCurrentLink: String;
var
  HTTPClient: TFPHTTPClient;
  HTMLContentText: String;
  HTMLContentStream: TStringStream;
  Doc: THTMLDocument;
  NodeList: TDOMNodeList;
  I: Integer;
begin
  Result:='';

  try
    HTTPClient:=TFPHTTPClient.Create(nil);
    HTMLContentText:=HTTPClient.SimpleGet('https://www.torproject.org/download/tor/');
  finally
    HTTPClient.Free;
    Exit;
  end;

  HTMLContentStream:=TStringStream.Create(HTMLContentText);
  Doc:=THTMLDocument.Create;
  ReadHTMLFile(Doc, HTMLContentStream);

  NodeList:=Doc.GetElementsByTagName('a');
  for I:=0 to NodeList.Count-1 do
  begin
    if String(TDOMElement(NodeList[I]).GetAttribute('href')).StartsWith('/dist/torbrowser/') then
    begin
      Result:=String(TDOMElement(NodeList[I]).GetAttribute('href'));
      break;
    end;
  end;

  NodeList.Free;
  Doc.Free;
  HTMLContentStream.Free;
end;

function TorCurrentVersion(CurrentLink: String): String;
var
  S, E: Integer;
begin
  if CurrentLink = '' then
  begin
    Result:='';
    Exit;
  end;

  S:=CurrentLink.IndexOf('win32-');
  S:=S+Length('win32-');
  E:=CurrentLink.IndexOf('.zip');
  Result:=CurrentLink.Substring(S, E-S);
end;

function TorGetProcessEntry(out PEOut: TProcessEntry32): boolean;
var
  Snapshot: THANDLE;
  PE: TProcessEntry32;
begin
  Result:=False;
  Snapshot:=CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  PE.dwSize:=SizeOf(PE);
  if Process32First(Snapshot, PE) then
  begin
    repeat
      if String(PE.szExeFile) = 'tor.exe' then
      begin
        PEOut:=PE;
        Result:=True;
        Break;
      end;
    until not Process32Next(Snapshot, PE);
  end;
  CloseHandle(Snapshot);
end;

function TorRunning: Boolean;
var
  PE: TProcessEntry32;
begin
  Result:=TorGetProcessEntry(PE);
end;

procedure TorStop;
var
  PE: TProcessEntry32;
  PH: THANDLE;
begin
  if TorGetProcessEntry(PE) then
  begin
    PH:=OpenProcess(PROCESS_TERMINATE, False, PE.th32ProcessID);
    TerminateProcess(PH, 0);
  end;
end;

function TorVersion: String;
var
  AProcess: TProcess;
  Output: String;
  StdOut: TStringList;
  S, E: Integer;
begin
  if not FileExists(TorBaseFolder+'tor\Tor\tor.exe') then
  begin
    Result:='';
    Exit;
  end;

  AProcess:=TProcess.Create(nil);
  AProcess.Executable:=TorBaseFolder+'tor\Tor\tor.exe';
  AProcess.Parameters.Add('--version');
  AProcess.Options:=AProcess.Options+[poUsePipes];
  AProcess.ShowWindow:=swoHIDE;
  AProcess.Execute;
  StdOut:=TStringList.Create;
  StdOut.LoadFromStream(AProcess.Output);
  Aprocess.Free;

  Output:=StdOut[0];
  StdOut.Free;

  S:=Output.IndexOf('version ');
  S:=S+Length('version ');
  E:=Output.IndexOf(' (');
  Result:=Output.Substring(S, E-S);
end;

procedure TorUpdate;
var
  HTTPClient: TFPHTTPClient;
  UnZipper: TUnZipper;
begin
  HTTPClient:=TFPHTTPClient.Create(nil);
  HTTPClient.AllowRedirect:=True;
  HTTPClient.Get('https://www.torproject.org'+TorCurrentLink, TorBaseFolder + 'tor.zip');
  HTTPClient.Free;

  UnZipper:=TUnZipper.Create;
  UnZipper.FileName:=TorBaseFolder + 'tor.zip';
  UnZipper.OutputPath:=TorBaseFolder + 'tor';
  UnZipper.UnZipAllFiles;
  UnZipper.Free;
end;

end.

