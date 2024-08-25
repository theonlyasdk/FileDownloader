unit MainForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType, FormAbout, fphttpclient, openssl, opensslsockets, strutils, FormDownloadProgress, uDownload, LCL, ExtCtrls, Variants, ComCtrls, VersionSupport;

type

    { TFMain }

    TFMain = class(TForm)
        BtnChooseFilePath: TButton;
        BtnClose:   TButton;
        BtnDownload: TButton;
        LblSavePath: TLabel;
        LblTitle:   TLabel;
        LblUrl:     TLabel;
        LblVersion: TLabel;
        LEUrl:      TEdit;
        LESavePath: TEdit;
        SaveFolderDialog: TSelectDirectoryDialog;
        procedure BtnChooseFilePathClick(Sender: TObject);
        procedure BtnCloseClick(Sender: TObject);
        procedure BtnDownloadClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LblTitleClick(Sender: TObject);
        procedure LEUrlChange(Sender: TObject);
    private

    public
        DlProgressForm: TFDownloadProgress;
        FDownload:      TDownload;
        procedure DetectFileNameFromUrl();
        function ValidateFields(): String;
        function PerformDownload(): Boolean;
        procedure DoOnDownloadProgress(Sender: TObject; {%H-}AFrom, ATo: String; APos, ASize, AElapsed, ARemaining, ASpeed: Longint);
        procedure DoOnDownloadCompleted(Sender: TObject);
        procedure DoOnDownloadError(Sender: TObject; const AErrMsg: String);
        procedure DownloadFormCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
        procedure SetButtonsEnabled(State: boolean);
    end;

var
    FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

function GetFileNameFromURL(URL: String): String;
var
    UrlComponents: TStringArray;
begin
    UrlComponents := SplitString(URL, '/');
    Result := UrlComponents[High(UrlComponents)];
end;

function DownloadHTTP(URL, TargetFile: String): Boolean;
var
    Client: TFPHttpClient;
    FS:     TStream;
    SL:     TStringList;
begin
    Result := False;

    InitSSLInterface;

    Client := TFPHttpClient.Create(nil);
    FS := TFileStream.Create(TargetFile, fmCreate or fmOpenWrite);
    try
        try
            { Allow redirections }
            Client.AllowRedirect := True;
            Client.Get(URL, FS);
        except
            on E: EHttpClient do
                Application.MessageBox(PChar(E.Message), 'Error while downloading', MB_OK + MB_ICONERROR);
            else
                raise;
        end;
    finally
        FS.Free;
        Client.Free;
    end;

    { Test our file }
    if FileExists(TargetFile) then
    try
        Result := True;
    finally
        SL.Free;
    end;
end;

function FormatSpeed(Speed: Longint): String;
const
    KB = 1024;
    MB = 1024 * KB;
    GB = 1024 * MB;
begin
    if Speed < KB then
        Result := FormatFloat('#,##0 bits/s', Speed)
    else if Speed < MB then
        Result := FormatFloat('#,##0.0 kB/s', Speed / KB)
    else if Speed < GB then
        Result := FormatFloat('#,##0.0 MB/s', Speed / MB)
    else
        Result := FormatFloat('#,##0.0 GB/s', Speed / GB);
end;

function SecToHourAndMin(const ASec: Longint): String;
var
    Hour, Min, Sec: Longint;
begin
    Hour := Trunc(ASec / 3600);
    Min := Trunc((ASec - Hour * 3600) / 60);
    Sec := ASec - Hour * 3600 - 60 * Min;
    Result := IntToStr(Hour) + 'h: ' + IntToStr(Min) + 'm: ' + IntToStr(Sec) + 's';
end;

function FormatSize(Size: Int64): String;
const
    KB = 1024;
    MB = 1024 * KB;
    GB = 1024 * MB;
begin
    if Size < KB then
        Result := FormatFloat('#,##0 Bytes', Size)
    else if Size < MB then
        Result := FormatFloat('#,##0.0 KB', Size / KB)
    else if Size < GB then
        Result := FormatFloat('#,##0.0 MB', Size / MB)
    else
        Result := FormatFloat('#,##0.0 GB', Size / GB);
end;

procedure TFMain.DoOnDownloadProgress(Sender: TObject; {%H-}AFrom, ATo: String; APos, ASize, AElapsed, ARemaining, ASpeed: Longint);
begin
    DlProgressForm.DlProgress.Style := pbstNormal;
    DlProgressForm.LblDownloading.Caption:='Downloading...';
    DlProgressForm.Caption := Concat('Downloading: ', ExtractFileName(ATo));
    DlProgressForm.LblDlSpeed.Caption := FormatSpeed(ASpeed);
    DlProgressForm.LblDlSpeed.Update;
    DlProgressForm.LblTimeElapsed.Caption := Concat('Elapsed time: ', SecToHourAndMin(AElapsed));
    DlProgressForm.LblTimeElapsed.Update;
    if ASize > 0 then
        DlProgressForm.LblRemaining.Caption := Concat('Remaining time: ', SecToHourAndMin(ARemaining))
    else
        DlProgressForm.LblRemaining.Caption := 'Unknown';
    DlProgressForm.LblRemaining.Update;
    if ASize > 0 then
        DlProgressForm.LblRecieved.Caption := 'Received: ' + FormatSize(APos) + ' / ' + FormatSize(ASize)
    else
        DlProgressForm.LblRecieved.Caption := 'Received: ' + FormatSize(APos) + ' / ' + 'Unknown';
    DlProgressForm.LblRecieved.Update;
    if ASize > 0 then
        DlProgressForm.DlProgress.Position := Round((APos / ASize) * 100);
    DlProgressForm.DlProgress.Update;
    Application.ProcessMessages;
end;

procedure TFMain.DoOnDownloadCompleted(Sender: TObject);
begin
    SetButtonsEnabled(True);
    Application.MessageBox(PChar(Concat('The file has been downloaded successfully to: ', LESavePath.Text)), 'Download success.', MB_OK + MB_ICONINFORMATION);
    DlProgressForm.Close;
end;

procedure TFMain.DoOnDownloadError(Sender: TObject; const AErrMsg: String);
begin
    Application.MessageBox(PChar(Concat('Unable to download requested file: ', AErrMsg)), 'Download failed!', MB_OK+MB_ICONERROR);
    SetButtonsEnabled(True);
    DlProgressForm.Close;
end;

procedure TFMain.SetButtonsEnabled(State: boolean);
begin
    BtnClose.Enabled := state;
    BtnDownload.Enabled := state;
    BtnChooseFilePath.Enabled:= state;
end;

procedure TFMain.DownloadFormCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
begin
     SetButtonsEnabled(True);
end;

function TFMain.PerformDownload(): Boolean;
begin
    DlProgressForm := TFDownloadProgress.Create(Self);
    SetButtonsEnabled(False);
    FDownload := TDownload.Create;
    FDownload.OnDownloadProgress := @DoOnDownloadProgress;
    FDownload.OnDownloadCompleted := @DoOnDownloadCompleted;
    DlProgressForm.OnClose:=@DownloadFormCloseEvent;
    DlProgressForm.Show;
    FDownload.DownloadFile(LEUrl.Text, LESavePath.Text);
    Result := True;
end;

procedure TFMain.DetectFileNameFromUrl();
begin
    LESavePath.Text := ConcatPaths([SaveFolderDialog.FileName, GetFileNameFromURL(LEUrl.Text)]);
end;

function TFMain.ValidateFields(): String;
begin
    if LEUrl.Text = '' then
    begin
        Result := 'URL should not be empty!';
        Exit;
    end;
    if LESavePath.Text = '' then
    begin
        Result := 'Save path should not be empty';
        Exit;
    end;
    Result := '';
end;

procedure TFMain.BtnChooseFilePathClick(Sender: TObject);
begin
    if SaveFolderDialog.Execute then
    begin
        LESavePath.Text := SaveFolderDialog.FileName;
        DetectFileNameFromUrl();
    end;
end;

procedure TFMain.BtnCloseClick(Sender: TObject);
begin
    Close;
end;

procedure TFMain.BtnDownloadClick(Sender: TObject);
var
    ValidationResult: String;
begin
    ValidationResult := ValidateFields();
    if ValidationResult <> '' then
    begin
        Application.MessageBox(PChar(ValidationResult), 'Validation Error', MB_OK + MB_ICONERROR);
        Exit;
    end;
    PerformDownload();
end;

procedure TFMain.FormShow(Sender: TObject);
begin
    SaveFolderDialog.FileName := ConcatPaths([GetUserDir, 'Downloads']);
    LblVersion.Caption:=GetFileVersion;
    DetectFileNameFromUrl();
end;

procedure TFMain.LblTitleClick(Sender: TObject);
var
    AForm: TFAbout;
begin
    AForm := TFAbout.Create(self);
    AForm.ShowModal;
end;

procedure TFMain.LEUrlChange(Sender: TObject);
begin
    DetectFileNameFromUrl();
end;

end.
