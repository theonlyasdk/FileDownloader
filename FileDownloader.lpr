program FileDownloader;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
  cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
  athreads,
    {$ENDIF}
    Classes,
    Interfaces, // this includes the LCL widgetset
    Forms,
    SysUtils,
    Dialogs,
    LCLType,
    Controls,
    Zipper,
    fphttpclient,
    MainForm,
    FormAbout,
    FormDownloadProgress,
    uDownload,
    uDarkStyleParams,
    uMetaDarkStyle,
    uDarkStyleSchemes,
    VersionSupport;

    {$R *.res}

var
    OpenSSLCheckResult: string;

function IsOpenSSLAvailable: boolean;
const
    {$IFDEF WIN64}
    cOpenSSLURL = 'http://packages.lazarus-ide.org/openssl-1.0.2j-x64_86-win64.zip';
    {$ENDIF}
    {$IFDEF WIN32}
    cOpenSSLURL = 'http://packages.lazarus-ide.org/openssl-1.0.2j-i386-win32.zip';
    {$ENDIF}
var
    {$IFDEF MSWINDOWS}
    UnZipper: TUnZipper;
    FHTTPClient: TFPHTTPClient;
    ParamPath, LibeayDLL, SsleayDLL, ZipFile: string;
    {$EndIf}
begin
    {$IFDEF MSWINDOWS}
    ParamPath := ExtractFilePath(ParamStr(0));
    LibeayDLL := ParamPath + 'libeay32.dll';
    SsleayDLL := ParamPath + 'ssleay32.dll';
    Result := FileExists(Libeaydll) and FileExists(Ssleaydll);
    if not Result then
    begin
        ZipFile := ParamPath + ExtractFileName(cOpenSSLURL);
        FHTTPClient := TFPHTTPClient.Create(nil);
        try
            try
                FHTTPClient.Get(cOpenSSLURL, ZipFile);
            except
            end;
        finally
            FHTTPClient.Free;
        end;
        if FileExists(ZipFile) then
        begin
            UnZipper := TUnZipper.Create;
            try
                try
                    UnZipper.FileName := ZipFile;
                    UnZipper.Examine;
                    UnZipper.UnZipAllFiles;
                except
                end;
            finally
                UnZipper.Free;
            end;
            DeleteFile(ZipFile);
            Result := FileExists(Libeaydll) and FileExists(Ssleaydll);
        end;
    end;
    {$ELSE}
      Result  := True;
    {$ENDIF}
end;


function CheckOpenSSLDynLibsPresent(): string;
begin
    Result := '';

    if not FileExists('libeay32.dll') then
        Result := 'Unable to load libeay32.dll! ';

    if not FileExists('ssleay32.dll') then
        Result := 'Unable to load ssleay32.dll! ';
end;

begin
    OpenSSLCheckResult := CheckOpenSSLDynLibsPresent();

    if OpenSSLCheckResult <> '' then
    begin
        Application.MessageBox('OpenSSL libraries are not available. The application will now attempt to download them.', 'OpenSSL libraries not installed.', MB_OK + MB_ICONINFORMATION);
        if not IsOpenSSLAvailable then MessageDlg('Unable to download OpenSSL libraries. You might have to download them manually. You should get: ssleay32.dll and libeay32.dll and put them in the application folder.', mtInformation, [mbOK], 0);
    end;

    RequireDerivedFormResource := True;
  Application.Scaled:=True;
    PreferredAppMode := pamAllowDark;
    uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
    Application.Initialize;
    Application.CreateForm(TFMain, FMain);
    Application.CreateForm(TFAbout, FAbout);
    Application.CreateForm(TFDownloadProgress, FDownloadProgress);
    Application.Run;
end.
