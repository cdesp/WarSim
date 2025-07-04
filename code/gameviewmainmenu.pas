{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display main menu. }
unit GameViewMainMenu;

interface


uses Classes,
  CastleControls, CastleWindow, CastleUIControls;

Const
  Version = 'Version 0.31';

type
  TViewMainMenu = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonPlayHexagonal: TCastleButton;
    ButtonQuit: TCastleButton;
    LblVersion: TCastleLabel;
  strict private
    procedure ClickHexagonal(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewMainMenu: TViewMainMenu;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleApplicationProperties,
  GameViewPlay;

constructor TViewMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmainmenu.castle-user-interface';
end;

procedure TViewMainMenu.Start;
begin
  inherited;
  ButtonPlayHexagonal.OnClick := {$ifdef FPC}@{$endif}ClickHexagonal;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif}ClickQuit;
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
  LblVersion.Text.Text := Version;
end;

procedure TViewMainMenu.ClickHexagonal(Sender: TObject);
begin
  ViewPlay.MapName := 'map-hexagonal';
  Container.View := ViewPlay;
end;


procedure TViewMainMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
