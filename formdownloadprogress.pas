unit FormDownloadProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TFDownloadProgress }

  TFDownloadProgress = class(TForm)
    BtnCancel: TButton;
    DlProgress: TProgressBar;
    LblDlSpeed: TLabel;
    LblDownloading: TLabel;
    LblRecieved: TLabel;
    LblRemaining: TLabel;
    LblTimeElapsed: TLabel;
    procedure BtnCancelClick(Sender: TObject);
  private

  public

  end;

var
  FDownloadProgress: TFDownloadProgress;

implementation

{$R *.lfm}

{ TFDownloadProgress }

procedure TFDownloadProgress.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

