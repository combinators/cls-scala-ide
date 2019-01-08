package org.combinators.cls.ide.examples.planning

import org.combinators.cls.types.{Kinding, Type, Variable}
import org.combinators.cls.types.syntax._
import org.combinators.cls.interpreter.combinator


class Repository {
  lazy val LS = Variable("LS")
  lazy val ST = Variable("ST")
  lazy val EG = Variable("EG")
  lazy val LK = Variable("LK")
  lazy val PB = Variable("PB")
  lazy val BI = Variable("BI")
  lazy val SW = Variable("SW")
  lazy val MO = Variable("MO")
  lazy val BG = Variable("BG")
  lazy val RI = Variable("RI")
  lazy val FR = Variable("FR")
  lazy val DR = Variable("DR")
  lazy val MA = Variable("MA")


  lazy val kinding: Kinding =

    Kinding(LS)
      .addOption('TruLaser_co2).addOption('TruLaser_fibre).addOption('TruLaser_Center).addOption('TruMatic)
      .merge(Kinding(ST).addOption('TruPunch).addOption('Scherschneidemaschine).addOption('TruMatic))
      .merge(Kinding(EG).addOption('Gleitschleifmaschine).addOption('Handschleifmaschine))
      .merge(Kinding(LK).addOption('Lakierkabine))
      .merge(Kinding(PB).addOption('Pulverbeschichtungsanlage))
      .merge(Kinding(BI).addOption('TruBench).addOption('TruBench_Tool).addOption('TruBenchCell))
      .merge(Kinding(SW).addOption('TruWeldCell).addOption('TruWeldRobot))
      .merge(Kinding(MO).addOption('Montagearbeitsplatz))
      .merge(Kinding(BG).addOption('TruMatic).addOption('Handarbeitsplatz_BG))
      .merge(Kinding(RI).addOption('Richtmaschine))
      .merge(Kinding(FR).addOption('Fraesmaschine))
      .merge(Kinding(DR).addOption('TruPrint))
      .merge(Kinding(MA).addOption('TruMark))


  @combinator object Contracts {
    def apply(SupplyLotSaleAgreement: String,
              SupplyContractPlans: String,
              SupplyContractSpecifications: String,
              SupplyContractSitePlan: String,
              SecureFinancing: String,
              ConstructionLoanSettlement: String): String = "Contracts"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyLotSaleAgreement =>: 'SupplyContractPlans =>: 'SupplyContractSpecifications =>: 'SupplyContractSitePlan =>: 'SecureFinancing =>: 'ConstructionLoanSettlement =>: 'Contracts

  }

  @combinator object AltContracts {
    def apply(SupplyLotSaleAgreement: String,
              SupplyContractPlans: String,
              SupplyContractSpecifications: String,
              ConstructionLoanSettlement: String): String = "Contracts"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyLotSaleAgreement =>: 'SupplyContractPlans =>: 'SupplyContractSpecifications =>: 'ConstructionLoanSettlement =>: 'Contracts

  }

  @combinator object SupplyLotSaleAgreement {
    def  apply:String = "SupplyLotSaleAgreement"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyLotSaleAgreement
  }

  @combinator object SecureFinancing {
    def  apply:String = "SecureFinancing"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SecureFinancing
  }

  @combinator object ConstructionLoanSettlement {
    def  apply:String = "ConstructionLoanSettlement"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'ConstructionLoanSettlement
  }

  @combinator object SupplyConstructionAgreement {
    def  apply:String = "SupplyConstructionAgreement"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyConstructionAgreement
  }

  @combinator object SupplyContractPlans {
    def apply(SupplyConstructionAgreement: String): String = "SuppylContractPlans"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyConstructionAgreement =>: 'SupplyContractPlans
  }

  @combinator object SuppylContractSpecifications {
    def apply(SupplyConstructionAgreement: String): String = "SupplyContractSpecifications"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyConstructionAgreement =>: 'SupplyContractSpecifications
  }

  @combinator object SupplyContractSitePlan {
    def apply(SupplyConstructionAgreement: String): String = "SupplyContractSitePlan"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'SupplyConstructionAgreement =>: 'SupplyContractSitePlan
  }

  @combinator object DocumentReviewAndRevision {

    def apply(ApproveRevisedPlans: String,
              ApproveRevisedSepcifications: String,
              ApproveRevisedSitePlan: String): String = "Document Review and Revision"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'ApproveRevisedPlans =>: 'ApproveRevisedSpecifications =>: 'ApproveRevisedSitePlan =>: 'DocumentReviewAndRevision
  }

  @combinator object DocumentReviewAndRevisionIteration {

    def apply(BuildingPermitNotApproved: String): String = "Document Review and Revision"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 25
    val Costs: Int = 0
    val Res: String = null
    val semanticType: Type = 'BuildingPermitNotApproved =>: 'DocumentReviewAndRevision
  }

  @combinator object ApproveRevisedPlans{
    def apply(ReviewAndFinalizePlans: String,
              PrintConstructionDrawings:String):String = "ApproveRevisedPlans"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type ='ReviewAndFinalizePlans =>: 'PrintConstructionDrawings =>: 'ApproveRevisedPlans
  }


  @combinator object ApproveRevisedSpecifications{
    def apply(ReviewAndFinalizeSpecifications: String,
              PrintConstructionDrawings:String):String = "ApproveRevisedSpecifications"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'ReviewAndFinalizeSpecifications =>: 'PrintConstructionDrawings =>: 'ApproveRevisedSpecifications
  }

  @combinator object ApproveRevisedSitePlan{
    def apply(ReviewAndFinalizeSitePlan: String,
              PrintConstructionDrawings:String):String = "ApproveRevisedSitePlan"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'ReviewAndFinalizeSitePlan =>: 'PrintConstructionDrawings =>: 'ApproveRevisedSitePlan
  }

  @combinator object PrintConstructionDrawings{
    def  apply: String = "Print"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 5
    val Costs: Int = 15431
    val Res: String = "Builder"

    val semanticType: Type = 'PrintConstructionDrawings
  }

  @combinator object ReviewAndFinalizePlans{
    def apply(SupplyContractPlans: String):String = "ReviewAndFinalizePlans"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 15
    val Costs: Int = 24094
    val Res: String = "Builder"

    val semanticType: Type = 'SupplyContractPlans =>: 'ReviewAndFinalizePlans
  }

  @combinator object ReviewAndFinalizeSpecifications{
    def apply(SupplyContractPlans: String):String = "ReviewAndFinalizeSpecifications"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 20
    val Costs: Int = 19599
    val Res: String = "Builder"
    val semanticType: Type = 'SupplyContractSpecifications =>: 'ReviewAndFinalizeSpecifications
  }

  @combinator object ReviewAndFinalizeSitePlan{
    def apply(SupplyContractPlans: String):String = "ReviewAndFinalizeSitePlan"


    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 22776
    val Res: String = "Builder"

    val semanticType: Type = 'SupplyContractSitePlan =>: 'ReviewAndFinalizeSitePlan
  }

  @combinator object ReviewAndFinalizePlansContract{
    def apply(Contracts: String):String = "ReviewAndFinalizePlans"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 15
    val Costs: Int = 3124
    val Res: String = "Builder"

    val semanticType: Type = 'Contracts =>: 'ReviewAndFinalizePlans
  }

  @combinator object ReviewAndFinalizeSpecificationsContract{
    def apply(Contracts: String):String = "ReviewAndFinalizeSpecifications"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 20
    val Costs: Int = 2363
    val Res: String = "Builder"

    val semanticType: Type = 'Contracts =>: 'ReviewAndFinalizeSpecifications
  }

  @combinator object ReviewAndFinalizeSitePlanContract{
    def apply(Contracts: String):String = "ReviewAndFinalizeSitePlan"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 19421
    val Res: String = "Builder"

    val semanticType: Type = 'Contracts =>: 'ReviewAndFinalizeSitePlan
  }

  @combinator object BidsAndContracts{
    def apply(Contracts: String,
              DocumentReviewAndRevision: String):String = "BidsAndContracts"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 24
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'Contracts =>: 'DocumentReviewAndRevision =>: 'BidsAndContracts
  }

  @combinator object GradingAndBuildingPermits{
    def apply(BuildingPermitIssued: String):String = "GradingAndBuildingPermits"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'BuildingPermitIssued =>: 'GradingAndBuildingPermits
  }


  @combinator object BuildingPermitIssued{
    def apply(PayPermitFeesAndExciseTaxes: String):String = "BuildingPermitIssued"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'PayPermitFeesAndExciseTaxes =>: 'BuildingPermitIssued
  }

  @combinator object PayPermitFeesAndExciseTaxes{
    def apply(BuildingPermitApproved: String):String = "PayPermitFeesAndExciseTaxes"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 23598
    val Res: String = "Builder"
    val semanticType: Type = 'BuildingPermitApproved =>: 'PayPermitFeesAndExciseTaxes
  }

  @combinator object BuildingPermitApproved{
    def apply(CountyPermitProcess:String,
              GradingPermitIssued: String):String = "BuildingPermitApproved"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 4904
    val Res: String = "Department of Permits and Licenses"
    val semanticType: Type = 'CountyPermitProcess =>: 'GradingPermitIssued =>: 'BuildingPermitApproved
  }

  @combinator object BuildingPermitNotApproved{
    def apply(CountyPermitProcess:String,
              GradingPermitIssued: String):String = "BuildingPermitNotApproved"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 12
    val Costs: Int = 0
    val Res: String = "Department of Permits and Licenses"
    val semanticType: Type = 'CountyPermitProcess =>: 'GradingPermitIssued =>: 'BuildingPermitNotApproved
  }

  @combinator object GradingPermitIssued{
    def apply(SedimentControlInsp:String):String = "GradingPermitIssued"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 16246
    val Res: String = "Sediment Control Inspector"
    val semanticType: Type = 'SedimentControlInsp =>: 'GradingPermitIssued
  }

  @combinator object SedimentControlInsp{
    def apply(InstallConstructionEntrance:String,
              InstallSedimentControls: String):String = "SedimentControlInsp"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 6896
    val Res: String = "Sediment Control Inspector"
    val semanticType: Type = 'InstallConstructionEntrance =>: 'InstallSedimentControls =>: 'SedimentControlInsp
  }

  @combinator object InstallSedimentControls{
    def apply(MeetSedimentControlInspector:String,
              WalkLot: String):String = "InstallSedimentControls"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 2
    val Costs: Int = 20680
    val Res: String = "Excavation Subcontractor"
    val semanticType: Type = 'InstallConstructionEntrance =>: 'WalkLot =>: 'InstallSedimentControls
  }

  @combinator object InstallConstructionEntrance{
    def apply(MeetSedimentControlInspector:String,
              WalkLot: String):String = "InstallConstructionEntrance"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 21520
    val Res: String = "Excavation Subcontractor"
    val semanticType: Type = 'MeetSedimentControlInspector =>: 'WalkLot =>: 'InstallConstructionEntrance
  }

  @combinator object WalkLot{
    def apply(MeetSedimentControlInspector:String):String = "WalkLot"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 13679
    val Res: String = "Builder"
    val semanticType: Type = 'MeetSedimentControlInspector =>: 'WalkLot
  }

  @combinator object MeetSedimentControlInspector{
    def apply(FileGradingPermitApplication:String,
              StakeLot:String,
              PostLotIdentification:String):String = "MeetSedimentControlInspector"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 9562
    val Res: String = "Builder"
    val semanticType: Type = 'FileGradingPermitApplication =>: 'StakeLot =>: 'PostLotIdentification =>: 'MeetSedimentControlInspector
  }

  @combinator object PostLotIdentification{
    def apply(FileBuildingPermitApplication:String,
              StakeLot: String):String = "PostLotIdentification"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 23422
    val Res: String = "Builder"
    val semanticType: Type = 'FileBuildingPermitApplication =>: 'StakeLot =>: 'PostLotIdentification
  }

  @combinator object StakeLot{
    def apply(ScheduleLotStakeOut:String):String = "StakeLot"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 19267
    val Res: String = "Civil Engineer"
    val semanticType: Type = 'ScheduleLotStakeOut =>: 'StakeLot
  }

  @combinator object CountyPermitProcess{
    def apply(FileBuildingPermitApplication:String):String = "CountyPermitProcess"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 1323
    val Res: String = "Department of Permits and Licenses"
    val semanticType: Type = 'FileBuildingPermitApplication =>: 'CountyPermitProcess
  }

  @combinator object FileBuildingPermitApplication{
    def apply(BidsAndContracts:String):String = "FileBuildingPermitApplication"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 3
    val Costs: Int = 22460
    val Res: String = "Builder"
    val semanticType: Type = 'BidsAndContracts =>: 'FileBuildingPermitApplication
  }

  @combinator object FileGradingPermitApplication{
    def apply(BidsAndContracts:String):String = "FileGradingPermitApplication"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 21813
    val Res: String = "Builder"
    val semanticType: Type = 'BidsAndContracts =>: 'FileGradingPermitApplication
  }

  @combinator object ScheduleLotStakeOut{
    def apply(BidAandContracts:String):String = "ScheduleLotStakeOut"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 19257
    val Res: String = "Builder"
    val semanticType: Type = 'BidsAndContracts =>: 'ScheduleLotStakeOut
  }

  @combinator object Foundation{
    def apply(StripTopsoilAndStockpile:String,
              FoundationCertification: String,
              SetLintelsBoltsCapBlock:String,
              WaterproofingAndDrainTile:String):String = "Foundation"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'StripTopsoilAndStockpile =>: 'FoundationCertification =>: 'SetLintelsBoltsCapBlock =>: 'WaterproofingAndDrainTile =>: 'Foundation
  }

  @combinator object StripTopsoilAndStockpile{
    def apply(ClearLot:String):String = "StripTopsoilAndStockpile"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 24089
    val Res: String = "Excavation Subcontractor"
    val semanticType: Type = 'ClearLot =>: 'StripTopsoilAndStockpile
  }

  @combinator object ClearLot{
    def apply(GradingAndBuildingPermits:String):String = "ClearLot"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 3
    val Costs: Int = 2507
    val Res: String = "Excavation Subcontractor"
    val semanticType: Type = 'GradingAndBuildingPermits =>: 'ClearLot
  }

  @combinator object FoundationCertificationPrime{
    def apply(BuildBlockFoundation:String):String = "FoundationCertification"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'BuildBlockFoundation =>: 'FoundationCertification
  }

  @combinator object FoundationCertificationOption{
    def apply(FillBlockCoresWConcrete:String):String = "FoundationCertification"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'FillBlockCoresWConcrete =>: 'FoundationCertification
  }

  @combinator object SetLintelsBoltsCapBlock{
    def apply(SteelDelivery:String):String = "SetLintelsBoltsCapBlock"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 2
    val Costs: Int = 14255
    val Res: String = "Block Masonry Subcontractor"
    val semanticType: Type = 'SteelDelivery =>: 'SetLintelsBoltsCapBlock
  }

  @combinator object SteelDelivery{
    def apply(FillBlockCoresWConcrete:String):String = "SteelDelivery"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 19770
    val Res: String = "Steel Supplier"
    val semanticType: Type = 'FillBlockCoresWConcrete =>: 'SteelDelivery
  }

  @combinator object LumberDelivery{
    def apply(FillBlockCoresWConcrete:String):String = "LumberDelivery"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 14271
    val Res: String = "Lumber Supplier"
    val semanticType: Type = 'FillBlockCoresWConcrete =>: 'LumberDelivery
  }

  @combinator object FillBlockCoresWConcrete{
    def apply(BuildBlockFoundation:String):String = "FillBlockCoresWConcrete"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 2052
    val Res: String = "Block Masonry Subcontractor"
    val semanticType: Type = 'BuildBlockFoundation =>: 'FillBlockCoresWConcrete
  }

  @combinator object WaterproofingAndDrainTile{
    def apply(LumberDelivery:String):String = "WaterproofingAndDrainTile"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 18062
    val Res: String = "Waterproofing Subcontractor"
    val semanticType: Type = 'LumberDelivery =>: 'WaterproofingAndDrainTile
  }

  @combinator object BuildBlockFoundation{
    def apply(PinFootings:String,
              StockBlockMortarSand:String):String = "BuildBlockFoundation"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 15
    val Costs: Int = 16847
    val Res: String = "Black Masonry Subcontractor"
    val semanticType: Type = 'PinFootings =>: 'StockBlockMortarSand =>: 'BuildBlockFoundation
  }

  @combinator object StockBlockMortarSand{
    def apply(PinFootings:String):String = "StockBlockMortarSand"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 13424
    val Res: String = "Black Masonry Subcontractor"
    val semanticType: Type = 'PinFootings =>: 'StockBlockMortarSand
  }

  @combinator object PinFootings{
    def apply(PourFootings:String):String = "PinFootings"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 19029
    val Res: String = "Civil Engineer"
    val semanticType: Type = 'PourFootings =>: 'PinFootings
  }

  @combinator object PourFootings{
    def apply(FootingInspection:String):String = "PourFootings"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 5012
    val Res: String = "Concrete Subcontractor"
    val semanticType: Type = 'FootingInspection =>: 'PourFootings
  }

  @combinator object FootingInspection{
    def apply(DigFootingsAndInstallReinforcing:String):String = "FootingInspection"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'DigFootingsAndInstallReinforcing =>: 'FootingInspection
  }

  @combinator object DigFootingsAndInstallReinforcing{
    def apply(LayoutFootings:String):String = "DigFootingsAndInstallReinforcing"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 2515
    val Res: String = "Concrete Subcontractor"
    val semanticType: Type = 'LayoutFootings =>: 'DigFootingsAndInstallReinforcing
  }

  @combinator object LayoutFootings{
    def apply(SiteWork:String):String = "LayoutFootings"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 5416
    val Res: String = "Concrete Subcontractor"
    val semanticType: Type = 'SiteWork =>: 'LayoutFootings
  }

  @combinator object ExcavateForFoundation{
    def apply(ClearLot:String,
              StakeLotForExcavation:String,
              RoughGradeLot:String):String = "ExcavateForFoundation"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 2
    val Costs: Int = 23705
    val Res: String = "Excavation Subcontractor"
    val semanticType: Type = 'ClearLot =>: 'StakeLotForExcavation =>: 'RoughGradeLot =>: 'ExcavateForFoundation
  }

  @combinator object RoughGradeLot{
    def apply(ClearLot:String,
              StakeLotForExcavation:String):String = "RoughGradeLot"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 13470
    val Res: String = "Excavation Subcontractor"
    val semanticType: Type = 'ClearLot =>: 'StakeLotForExcavation =>: 'RoughGradeLot
  }

  @combinator object StakeLotForExcavation{
    def apply(ClearLot:String):String = "StakeLotForExcavation"
    //insert Atrributes and Graphcode Here
    val Duration: Int = 1
    val Costs: Int = 17371
    val Res: String = "Civil Engineer"
    val semanticType: Type = 'ClearLot =>: 'StakeLotForExcavation
  }

  @combinator object SiteWork{
    def apply(ExcavateForFoundation:String):String = "SiteWork"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null

    val semanticType: Type = 'ExcavateForFoundation =>: 'SiteWork
  }

  @combinator object FullProject{
    def apply(Contracts:String, DocumentReviewAndRevision: String, BidsAndContracts: String, GradingAndBuildingPermits: String, SiteWork: String, Foundation: String): String = "FullProject"

    //insert Atrributes and Graphcode Here
    val Duration: Int = 0
    val Costs: Int = 0
    val Res: String = null


    val semanticType: Type = 'Contracts =>: 'DocumentReviewAndRevision =>: 'BidsAndContracts =>: 'GradingAndBuildingPermits =>: 'SiteWork =>: 'Foundation =>: 'FullProject
  }

  @combinator object Laserschneiden_TruLaser_co2 {
    def apply: String = "TruLaser CO2"

    val capacity: Int = 49534
    val semanticType: Type = 'LS('TruLaser_co2)
  }

  @combinator object Laserschneiden_TruLaser_fibre {
    def apply: String = "TruLaser Fibre"

    val capacity: Int = 63099
    val semanticType: Type = 'LS('TruLaser_fibre)
  }

  @combinator object Laserschneiden_TruLaserCenter {
    def apply: String = "TruLaser Center"

    val capacity: Int = 84919
    val semanticType: Type = 'LS('TruLaserCenter)
  }

  @combinator object Laserschneiden_TruMatic {
    def apply: String = "TruMatic"

    val capacity: Int = 60160
    val semanticType: Type = 'LS('TruMatic)
  }

  @combinator object Stanzen_TruPunch {
    def apply: String = "TruPunch"

    val capacity: Int = 63649
    val semanticType: Type = 'ST('TruPunch)
  }

  @combinator object Stanzen_Scherschneidemaschine {
    def apply: String = "Scherschneidemaschine"

    val capacity: Int = 68986
    val semanticType: Type = 'ST('Scherschneidemaschine)
  }

  @combinator object Stanzen_TruMatic {
    def apply: String = "TruMatic"

    val capacity: Int = 74559
    val semanticType: Type = 'ST('TruMatic)
  }

  @combinator object Entgraten_Gleitschleifmaschine {
    def apply: String = "Gleitschleifmaschine"

    val capacity: Int = 53709
    val semanticType: Type = 'EG('Gleitschleifmaschine)
  }

  @combinator object Entgraten_Handschleifmaschine {
    def apply: String = "Handschleifmaschine"

    val capacity: Int = 84785
    val semanticType: Type = 'EG('Handschleifmaschine)
  }

  @combinator object Lakieren_Lakierkabine {
    def apply: String = "Lakierkabine"

    val capacity: Int = 71235
    val semanticType: Type = 'LK('Lakierkabine)
  }

  @combinator object Beschichten_Pulverbeschichtungsanlage {
    def apply: String = "Pulverbeschichtungsanlage"

    val capacity: Int = 59408
    val semanticType: Type = 'PB('Pulverbeschichtungsanlage)
  }

  @combinator object Biegen_TruBench {
    def apply: String = "TruBench"

    val capacity: Int = 69335
    val semanticType: Type = 'BI('TruBench)
  }

  @combinator object Biegen_TruBench_Tool {
    def apply: String = "TruBench_Tool"

    val capacity: Int = 79303
    val semanticType: Type = 'BI('TruBench_Tool)
  }

  @combinator object Biegen_TruBenchCell {
    def apply: String = "TruBenchCell"

    val capacity: Int = 67366
    val semanticType: Type = 'BI('TruBenchCell)
  }

  @combinator object Schweissen_TruWeldCell {
    def apply: String = "TruWeldCell"

    val capacity: Int = 77002
    val semanticType: Type = 'SE('TruWeldCell)
  }

  @combinator object Schweissen_TruWeldRobot {
    def apply: String = "TruWeldRobot"

    val capacity: Int = 65957
    val semanticType: Type = 'SE('TruWeldRobot)
  }

  @combinator object Montage_Montagearbeitsplatz {
    def apply: String = "Montagearbeitsplatz"

    val capacity: Int = 71042
    val semanticType: Type = 'MO('Montagearbeitsplatz)
  }

  @combinator object BohrenUndGewinde_TruMatic {
    def apply: String = "TruMatic"

    val capacity: Int = 60763
    val semanticType: Type = 'BG('TruMatic)
  }

  @combinator object BohrenUndGewinde_Handarbeitsplatz_BG {
    def apply: String = "Handarbeitsplatz_BG"

    val capacity: Int = 80284
    val semanticType: Type = 'BG('Handarbeitsplatz_BG)
  }

  @combinator object Richten_Richtmaschine {
    def apply: String = "Richtmaschine"

    val capacity: Int = 57964
    val semanticType: Type = 'RI('Richtmaschine)
  }

  @combinator object Fraesen_Fraesmaschine {
    def apply: String = "Fraesmaschine"

    val capacity: Int = 53407
    val semanticType: Type = 'FR('Fraesmaschine)
  }

  @combinator object Drucken_TruPrint {
    def apply: String = "TruPrint"

    val capacity: Int = 74981
    val semanticType: Type = 'DR('TruPrint)
  }

  @combinator object Markieren_TruMark {
    def apply: String = "TruMark"

    val capacity: Int = 75630
    val semanticType: Type = 'MA('TruMark)
  }

  @combinator object P1{
    def apply(LS:String, EG:String, BI1:String, BI2:String, BI3:String, SW:String, PB:String):String = "P1"

    val m_time: Int = 44;
    val s_time: Int = 258720;
    val stueckzahl: Int = 38;
    val semanticType: Type = 'LS(LS) =>: 'EG(EG) =>: 'BI(BI) =>: 'BI(BI) =>: 'BI(BI) =>: 'SW(SW) =>: 'PB(PB) =>: 'P1
  }

  @combinator object P2{
    def apply(LS:String, EG:String, BI1:String, BI2:String, LK:String):String = "P2"

    val p_time: Int = 45;
    val s_time: Int = 56700;
    val stueckzahl: Int = 21;
    val semanticType: Type = 'LS(LS) =>: 'EG(EG) =>: 'BI(BI) =>: 'BI(BI) =>: 'LK(LK) =>: 'P2
  }

  @combinator object P3{
    def apply(LS:String, EG:String, BI:String, BG:String, MO:String):String = "P3"

    val p_time: Int = 41;
    val s_time: Int = 76260;
    val stueckzahl: Int = 31;
    val semanticType: Type = 'LS(LS) =>: 'EG(EG) =>: 'BI(BI) =>: 'BG(BG) =>: 'MO(MO) =>: 'P3
  }

}

