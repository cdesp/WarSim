{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unit (soldier) on a map. }
unit GameUnit;

interface

uses Classes, Contnrs,
  CastleTransform, CastleComponentSerialize, CastleVectors, CastleScene,
  CastleTiledMap,System.Generics.Collections;

type
  TUnitKind = (ukAlienLight, ukAlienHeavy, ukHoplites, ukArchers, ukKnights);

  TUnit = class;
  TUnitList = TObjectList<TUnit>;
  TGroupList = TObjectDictionary<integer,TUnitList>;

  TUnitsOnMap = class(TComponent)
  private
    FItems: array of array of TUnitList;
    FUnits: TComponentList;
    FGroupList:TGroupList;
    FSelectedGroup:integer;
    procedure GroupSelect(GroupID: integer);
    procedure GroupUnSelect(GroupID: integer);
    procedure setSelectedGroup(const Value: integer);
    procedure setGroupSelect(GroupID:Integer; V: Boolean);
  strict private
    FMap: TCastleTiledMap;
    FUnitsCount: Integer;
    function GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
    function GetUnits(const Index: Integer): TUnit;
    procedure SetUnitsCount(const AValue: Integer);
  public
    constructor Create(const AOwner: TComponent;
      const AMap: TCastleTiledMap); reintroduce;
    destructor Destroy; override;

    { What unit is present at each map tile (@nil if none).

      You cannot change it directly, instead
      - add new TUnit,
      - destroy a TUnit,
      - change TUnit.TilePosition.

      In other words, changes to TUnit are automatically reflected here. }
    property Items[const TilePosition: TVector2Integer]: TUnit read GetUnitOnMap; default;

    function UnitsCount: Integer;
    function UnitsOnTile(const TilePosition: TVector2Integer):TUnitList;
    function NewGroup(GID:Integer):TUnitList;
    procedure ClearSelectedUntis;
    procedure DeleteGroup(GroupID:integer);
    Procedure AddSelectedUnitsToGroup(GroupId:Integer);
    property Units[const Index: Integer]: TUnit read GetUnits;
    property Map: TCastleTiledMap read FMap;
    property GroupList:TGroupList read FGroupList;
    property SelectedGroup:integer read FSelectedGroup write setSelectedGroup;
    function IsWater(const TilePosition: TVector2Integer): Boolean;
  end;

  TUnit = class(TComponent)
  private
    class var
      TransformTemplate: TCastleComponentFactory;
    procedure SetMovingPath(const Value: TList);
    function getFacingToTarget: Integer;
    procedure checkValidTile;
    function IsInGroup: Boolean;
    function getGroupID: Integer;
    procedure setGroupID(const Value: Integer);
    procedure setSelected(const Value: Boolean);
  var
      FSecondsPassed: Single;
      FSpeed: Integer;
      FTargetTile: TVector2Integer;
      FUnitFacing: Integer;
      FUnitFacingSave: Integer;
      FGroupID: Integer;
      procedure SetSecondsPassed(const Value: Single);
      procedure SetSpeed(const Value: Integer);
      procedure SetTargetTile(const Value: TVector2Integer);
      procedure SetUnitFacing(const Value: Integer);
  strict private
    var
      FKind: TUnitKind;
      FAttack, FLife, FMovement: Integer;
      FInitialMovement: Integer;
      ImageIcon: TCastleImageTransform;
      TextAttack: TCastleText;
      TextLife: TCastleText;
      TextMovement: TCastleText;
      Background: TCastleImageTransform;
      SelBox: TCastleBox;
      FTilePosition: TVector2Integer;
      FUnitsOnMap: TUnitsOnMap;
      FMovingPath : TList;
      FSelected: Boolean;
    procedure SetTilePosition(const Value: TVector2Integer);
    procedure SetUnitsOnMap(const Value: TUnitsOnMap);
    procedure PlaceOnMap;
    procedure RemoveFromMap;
    property UnitsOnMap: TUnitsOnMap read FUnitsOnMap write SetUnitsOnMap;
    procedure SetLife(const Value: Integer);
    procedure SetMovement(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Transform: TCastleTransform;
    FaceRotation: TVector4;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind; const AnAttack, ALife, AMovement: Integer); overload;
    { Initialize, choosing default statistics for this TUnitKind. }
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind); overload;
    function ToString: String; override;
    function Human: Boolean;

    { Can this unit move to given new position.
      Returns @true if the tile is empty and possible to walk into (not water),
      or occupied by the enemy (in which case we do attack).}
    function CanMove(const NewTilePosition: TVector2Integer): Boolean;
    procedure PathReached;
    procedure ClearGroup;
    function GroupUnitList:TUnitlist;

    property Kind: TUnitKind read FKind;
    property Attack: Integer read FAttack;
    property InitialMovement: Integer read FInitialMovement;
    { You can change unit life. Setting life to <= 0 frees the unit instance,
      removing it also from the map. }
    property Life: Integer read FLife write SetLife;
    property Movement: Integer read FMovement write SetMovement;
    property SecondsPassed: Single read FSecondsPassed write SetSecondsPassed;
    property Speed: Integer read FSpeed write SetSpeed;
    property TargetTile: TVector2Integer read FTargetTile write SetTargetTile;
    property TilePosition: TVector2Integer read FTilePosition write SetTilePosition;
    property UnitFacing: Integer read FUnitFacing write SetUnitFacing;
    property MovingPath:TList read FMovingPath write SetMovingPath;
    property InGroup:Boolean read IsInGroup;
    property GroupID:Integer read getGroupID write setGroupID;
    property Selected:Boolean read FSelected write setSelected;
  end;

const
  ZUnit = 300.0;
  ZHover = 200.0;

implementation

uses SysUtils, TypInfo, Math, windows,GameViewPlay, System.Types,
  CastleRectangles, CastleStringUtils, CastleColors, CastleViewport, pathfinderunit;

{ TUnitsOnMap ---------------------------------------------------------------- }

procedure TUnitsOnMap.setGroupSelect(GroupID:integer;V:Boolean);
var UList:TUnitList;
    i:integer;
begin
  if not Grouplist.TryGetValue(GroupID,UList) then exit;
  for i := 0 to UList.Count-1 do
     UList[i].Selected:=v;
end;

procedure TUnitsOnMap.GroupSelect(GroupID:integer);
begin
  setGroupSelect(GroupID,true);
  FSelectedGroup:=GroupID;
end;

procedure TUnitsOnMap.GroupUnSelect(GroupID:integer);
begin
  setGroupSelect(GroupID,false);
  FSelectedGroup:=-1;
end;

procedure TUnitsOnMap.AddSelectedUnitsToGroup(GroupId: Integer);
Var Ulist:TUnitList;
    i:integer;
begin
  if not Grouplist.ContainsKey(Groupid) then
   UList:=NewGroup(GroupID)
  else
   GroupList.TryGetValue(GroupID,UList);
  for i := 0 to UnitsCount-1 do
   if Units[i].Selected then
   begin
     UList.Add(Units[i]);
     Units[i].GroupID:=GroupId;
   end;

end;

constructor TUnitsOnMap.Create(const AOwner: TComponent;
  const AMap: TCastleTiledMap);
var i,j : integer;
begin
  inherited Create(AOwner);
  FMap := AMap;
  SetLength(FItems, Map.Data.Width, Map.Data.Height);
  for i := 0 to Map.Data.Width-1 do
    for j := 0 to Map.Data.Height-1 do
       FItems[i,j]:= TUnitlist.Create(False);
  FUnits := TComponentList.Create(false);
  FGroupList:=TGroupList.Create();
end;

procedure TUnitsOnMap.ClearSelectedUntis;
var i:integer;
begin
  for i:=0 to UnitsCount-1 do
    Units[i].Selected:=false;
end;

procedure TUnitsOnMap.DeleteGroup(GroupID: Integer);
Var UList:TUnitList;
    i:integer;
begin
  if GroupList.ContainsKey(GroupID) then
  begin
    //GroupUnSelect(GroupID);
    if Grouplist.TryGetValue(GroupID,UList) then
      for i :=UList.Count-1 downto 0 do
       UList[i].GroupID:=-1;
    GroupList.Remove(GroupID);//free UList automatically
  end;
end;

destructor TUnitsOnMap.Destroy;
begin
  FreeAndNil(FUnits);
  inherited;
end;

function TUnitsOnMap.GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
begin
  if FItems[TilePosition.X, TilePosition.Y].Count>0 then
    Result := FItems[TilePosition.X, TilePosition.Y].Items[0] //return by default the first object
  else
   Result := nil;
end;

function TUnitsOnMap.UnitsCount: Integer;
begin
  Result := FUnits.Count;
end;

function TUnitsOnMap.GetUnits(const Index: Integer): TUnit;
begin
  Result := FUnits[Index] as TUnit;
end;

function TUnitsOnMap.UnitsOnTile(
  const TilePosition: TVector2Integer): TUnitList;
begin
   Result := FItems[TilePosition.X, TilePosition.Y];
end;

procedure TUnitsOnMap.SetUnitsCount(const AValue: Integer);
begin
  if FUnitsCount = AValue then Exit;
  FUnitsCount := AValue;
end;

function TUnitsOnMap.IsWater(const TilePosition: TVector2Integer): Boolean;
var
  Tileset: TCastleTiledMapData.TTileset;
  Frame: Integer;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
begin
  if (TilePosition.X<0) Or (TilePosition.X>=map.Data.Width)  or
    (TilePosition.Y<0) Or (TilePosition.Y>=map.Data.Height) then
     exit(true);

  Result := Map.Data.TileRenderData(TilePosition,
    Map.Data.Layers[0],
    Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) and
    {inpassable is 6}
        (Frame in [5,6,7,10,11]);

    { Water is on 1, 5, 9 tiles (counting from 0) in data/maps/tileset-terrain.png . }
//    ((Frame mod 4) = 1);
end;

function TUnitsOnMap.NewGroup(GID:Integer): TUnitList;
begin
  Result:=TunitList.Create(false);
  GroupList.Add(GID,Result);
end;

procedure TUnitsOnMap.setSelectedGroup(const Value: integer);
begin
  if FSelectedGroup>-1 then
   GroupUnSelect(FSelectedGroup);
  FSelectedGroup := Value;
  GroupSelect(FSelectedGroup);
end;

{ TUnit ----------------------------------------------------------------------- }

function TUnit.GroupUnitList:TUnitlist;
begin
 if not UnitsOnMap.FGroupList.TryGetValue(FGroupID,Result) then
   Result:=nil;
end;

procedure TUnit.ClearGroup;
begin
  //removeunit from group
  if FGroupID>-1 then
    GroupUnitList.Remove(Self);
  FGroupID:=-1;
end;

constructor TUnit.Create(AOwner: TComponent);
begin
  inherited;
  if TransformTemplate = nil then
    TransformTemplate := TCastleComponentFactory.Create('castle-data:/unit.castle-transform');
  Transform := TransformTemplate.TransformLoad(Self);

  Background := FindRequiredComponent('Background') as TCastleImageTransform;
  ImageIcon := FindRequiredComponent('ImageIcon') as TCastleImageTransform;
  TextAttack := FindRequiredComponent('TextAttack') as TCastleText;
  TextLife := FindRequiredComponent('TextLife') as TCastleText;
  TextMovement := FindRequiredComponent('TextMovement') as TCastleText;
  SelBox := FindRequiredComponent('SelBox') as TCastleBox;

  FGroupID:=-1;
  UnitFacing:=0;
end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap;
  const AKind: TUnitKind;
  const AnAttack, ALife, AMovement: Integer);
const
  UnitIconUrls: array [TUnitKind] of string  =
  ( 'castle-data:/units/alien1.png',
    'castle-data:/units/alien2.png',
    'castle-data:/units/human1.png',
    'castle-data:/units/human2.png',
    'castle-data:/units/human3.png'
  );
begin
  FKind := AKind;
  FAttack := AnAttack;
  FLife := ALife;
  FInitialMovement := AMovement;
  FMovement := AMovement;

  // adjust components in Transform
  ImageIcon.URL := UnitIconUrls[AKind];
  // change Background.Color (RGB, leave alpha as it was)
  if Human then
    Background.Color := ColorOpacity(HexToColor('FFFFFF'), Background.Color.W)
  else
    Background.Color := ColorOpacity(HexToColor('FFFF00'), Background.Color.W);
  TextAttack.Caption := IntToStr(Attack);
  TextLife.Caption := IntToStr(Life);
  TextMovement.Caption := IntToStr(Movement);

  UnitsOnMap := AUnitsOnMap;
  UnitsOnMap.FUnits.Add(Self);

  PlaceOnMap;
end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap; const AKind: TUnitKind);
var
  Heavy: Boolean;
  AnAttack, ALife, AMovement: Integer;
begin
  Heavy := AKind in [ukAlienHeavy, ukHoplites];
  AnAttack := IfThen(Heavy, 7, 3);
  ALife := IfThen(Heavy, 20, 10);
  AMovement := IfThen(Heavy, 2, 5);
  Speed:= IfThen(Heavy, 40, 60); //pixels per second 256 pixels to cross a hex
  Initialize(AUnitsOnMap, AKind, AnAttack, ALife, AMovement);
end;

function TUnit.IsInGroup: Boolean;
begin
   Result:= GroupUnitList<>nil;
end;

destructor TUnit.Destroy;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FUnits.Remove(Self);
    RemoveFromMap;
    UnitsOnMap := nil;
  end;
  inherited;
end;

procedure TUnit.PlaceOnMap;
var
  R: TFloatRectangle;
  Scale: Single;
const
  { Must correspond to the unit.castle-transform original size (see in editor at design-time). }
  UnitDesignedSize = 256.0;
  { Additional unit scale to look better. }
  UnitScale = 0.8;
  { Additional unit scale to look better on Isometric maps. }
  UnitScaleIsometric = 0.75;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y].Add(Self);
    FTargetTile := TilePosition; //not moving

    R := UnitsOnMap.Map.TileRectangle(TilePosition);
    Transform.Translation := Vector3(R.Center, ZUnit);
    { The unit.castle-transform is designed for tile size = 256.
      Adjust it to match the actual tile size. }
    Scale := UnitScale * R.Height / UnitDesignedSize;
    Transform.Scale := Vector3(Scale, Scale, 1.0);

    Transform.Exists := true;
  end;
end;

procedure TUnit.RemoveFromMap;
begin
  if (UnitsOnMap <> nil) and
     UnitsOnMap.FItems[TilePosition.X, TilePosition.Y].Contains(Self) then
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y].Remove(Self);

  Transform.Exists := false;
end;

procedure TUnit.setGroupID(const Value: Integer);
begin
  //if we belong to another group we remove ourselfs
  ClearGroup;
  FGroupID:=Value;
end;

procedure TUnit.SetTilePosition(const Value: TVector2Integer);
begin
  RemoveFromMap;
  FTilePosition := Value;
  PlaceOnMap;
end;

function TUnit.ToString: String;
begin
  Result := Format('%s (Attack:%d, Life:%d, Movement:%d)', [
    SEnding(GetEnumName(TypeInfo(TUnitKind), Ord(Kind)), 3),
    Attack,
    Life,
    Movement
  ]);
end;

function TUnit.Human: Boolean;
begin
  Result := FKind in [ukHoplites, ukArchers, ukKnights];
end;

function TUnit.CanMove(const NewTilePosition: TVector2Integer): Boolean;
const
  { Both true and false work OK, change to determine
    whether you can move/attack along diagonals. }
  CornersAreNeighbors = true;
var
  UnitOnNewPosition: TUnit;
begin
  Result :=
    (UnitsOnMap <> nil)  and
    not UnitsOnMap.IsWater(NewTilePosition);
  exit;
  // cannot move over a unit of the same side
  if Result then
  begin
    UnitOnNewPosition := UnitsOnMap[NewTilePosition];
    if (UnitOnNewPosition <> nil) and (UnitOnNewPosition.Human = Human) then
      Exit(false);
  end;
end;

procedure TUnit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FUnitsOnMap) then
    { set to nil by SetUnitsOnMap to clean nicely }
    UnitsOnMap := nil;
end;

procedure TUnit.SetUnitsOnMap(const Value: TUnitsOnMap);
begin
  if FUnitsOnMap <> Value then
  begin
    if FUnitsOnMap <> nil then
      FUnitsOnMap.RemoveFreeNotification(Self);
    FUnitsOnMap := Value;
    if FUnitsOnMap <> nil then
      FUnitsOnMap.FreeNotification(Self);
  end;
end;


procedure TUnit.SetLife(const Value: Integer);
begin
  if FLife <> Value then
  begin
    FLife := Value;
    if Value > 0 then
      TextLife.Caption := IntToStr(Life)
    else
      Self.Destroy;
  end;
end;

procedure TUnit.SetMovement(const Value: Integer);
begin
  if FMovement <> Value then
  begin
    FMovement := Value;
    TextMovement.Caption := IntToStr(Movement);
  end;
end;


function TUnit.getFacingToTarget:Integer;
Begin
   Result := TPathFinder.GetHexDirection(Tile(TilePosition.X,TilePosition.Y),Tile(TargetTile.X,TargetTile.Y));
End;

function TUnit.getGroupID: Integer;
begin
   Result:=FGroupID;
end;


//Check if the tile we are is valid meaning no other unit occupied it
procedure TUnit.checkValidTile;
var un: TUnit;
    Newpos:TVector2Integer;

    function findposnear(Pos:TVector2Integer;dist:integer):boolean;
    Var AdjTiles: TTileArray;
        i:Integer;
    begin
      if dist=3 then exit(False); //up to distance 3

      AdjTiles:=TPathfinder.GetHexAdjTiles(Tile(Pos.X,pos.Y));
      for i := 0 to High(AdjTiles) do
      begin
        if (FUnitsOnMap.UnitsOnTile(Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y)).count=0) and
            not FUnitsOnMap.isWater(Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y))  then
        begin
          NewPos:=Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y);
          Exit(True);
        end;
      end;
      //here if no empty
      for i := 0 to High(AdjTiles) do
      begin
        Result:= findposnear(Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y),dist+1);
        If Result then
         break;
      end;
    end;

    procedure MoveToNewPos;
    Var pathf : TPathfinder;

    Begin
       pathf := TPathfinder.Create(TilePosition.X,TilePosition.Y, NewPos.X, NewPos.Y);
       pathf.OnIsTilePassable := ViewPlay.IsTilePassable;
       pathf.OnGetSpeed := ViewPlay.GetUnitSpeed;
       MovingPath := pathf.FindPath;  //list of ttile Records
       pathf.free;
    End;

begin
   Newpos:=TVector2Integer.Zero;
   if FUnitsonMap.UnitsOnTile(tileposition).Count=1 then exit;

   //find a position near
   for un in FUnitsonMap.UnitsOnTile(tileposition) do
   begin
     if un=self then
     begin
       if findposnear(TilePosition,1)  then
          MoveToNewPos
       else
         OutputDebugString('Can not find empty position!!!');
       break;
     end;
   end;
end;

procedure TUnit.PathReached;
var tile:TTile;
begin
   if Assigned(MovingPath) and (MovingPath.Count>0) then //next tile on path
   begin
     tile:=PathfinderUnit.TTile(MovingPath.Items[MovingPath.Count-1]^);
     TargetTile:= Vector2Integer(tile.X,tile.Y);
     MovingPath.Remove(MovingPath.Items[MovingPath.Count-1]);
     UnitFacing := getFacingToTarget;
   end
   else
    if Assigned(MovingPath) then  //Moving at path ended
    begin
      freeandnil(Movingpath);
      UnitFacing:= FUnitFacingSave;  //restore facing
      checkValidTile;
    end;

end;

procedure TUnit.SetMovingPath(const Value: TList);
begin
  FMovingPath := Value;
  if assigned(Value) then
  begin
    MovingPath.Remove(MovingPath.Items[MovingPath.Count-1]);
    FUnitFacingSave := UnitFacing; //save starting facing
    PathReached;
  end;
end;

procedure TUnit.SetSecondsPassed(const Value: Single);
begin
  FSecondsPassed := Value;
end;

procedure TUnit.setSelected(const Value: Boolean);
begin
  FSelected := Value;
  SelBox.Exists:=Value;
end;

procedure TUnit.SetSpeed(const Value: Integer);
begin
  FSpeed := Value;
end;

procedure TUnit.SetTargetTile(const Value: TVector2Integer);
begin
  FTargetTile := Value;
end;

procedure TUnit.SetUnitFacing(const Value: Integer);
begin
  if Value > 5 then
   FUnitFacing :=0
  else
   FUnitFacing := Value;

  case FUnitFacing of
   0:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(30));
     end;
   1:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(90));
     end;
   2:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(150));
     end;
   3:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(210));
     end;
   4:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(270));
     end;
   5:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(330));
     end;
  end;

 // ImageIcon.Rotation := FaceRotation;
 // Background.Rotation := FaceRotation;
 Transform.Rotation := FaceRotation;

end;

initialization

finalization
  FreeAndNil(TUnit.TransformTemplate);
end.
