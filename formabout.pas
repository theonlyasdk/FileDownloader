unit FormAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ATLinkLabel, FormOpenSourceLicenses;

type

  { TFAbout }

  TFAbout = class(TForm)
    BtnClose: TButton;
    LblDesc: TLabel;
    LblTitle: TLabel;
    LinkGitHub: TATLabelLink;
    MemoLicense: TMemo;
  private

  public

  end;

var
  FAbout: TFAbout;

implementation

{$R *.lfm}

end.

