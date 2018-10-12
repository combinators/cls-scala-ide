package org.combinators.cls.ide.examples.planning



import org.combinators.cls.types.{Kinding, Type, Variable}
import org.combinators.cls.types.syntax._
import Helpers._
import org.combinators.cls.interpreter.combinator


class Repository {
  lazy val alpha = Variable("alpha")
  lazy val beta = Variable("beta")
  lazy val theta = Variable("theta")

  lazy val kinding: Kinding =
    Kinding(alpha)


  @combinator object Contracts {
    def apply(SupplyLotSaleAgreement: String,
              SupplyContractPlans: String,
              SupplyContractSpecifications: String,
              SupplyContractSitePlan: String,
              SecureFinancing: String,
              ConstructionLoanSettlement: String): String = "Contracts"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'SupplyLotSaleAgreement =>: 'SupplyContractPlans =>: 'SupplyContractSpecificataions =>: 'SupplyContractSitePlan =>: 'SecureFinancing =>: 'ConstructionLoanSettlement =>: 'Contracts

  }

  @combinator object SupplyLotSaleAgreement {
    def  apply:String = "SupplyLotSaleAgreement"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'SupplyLotSaleAgreement
  }

  @combinator object SecureFinancing {
    def  apply:String = "SecureFinancing"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'SecureFinancing
  }

  @combinator object ConstructionLoanSettlement {
    def  apply:String = "ConstructionLoanSettlement"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'ConstructionLoanSettlement
  }

  @combinator object SupplyContractPlans {
    def apply(SupplyConstructionAgreement: String): String = "SuppylContractPlans"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'SupplyConstructionAgreement =>: 'SupplyContractPlans
  }

  @combinator object SuppylContractSpecifications {
    def apply(SupplyConstructionAgreement: String): String = "SupplyContractSpecifications"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'SupplyConstructionAgreement =>: 'SupplyContractSpecificataions
  }

  @combinator object SupplyContractSitePlan {
    def apply(SupplyConstructionAgreement: String): String = "SupplyContractSitePlan"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'SupplyConstructionAgreement =>: 'SupplyContractSitePlan
  }

  @combinator object DocumentReviewAndRevision {

    def apply(ApproveRevisedPlans: String,
              ApproveRevisedSepcifications: String,
              ApproveRevisedSitePlan: String): String = "Document Review and Revision"

    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'ApproveRevisedPlans =>: 'ApproveRevisedSecifications =>: 'ApproveRevisedSitePlan =>: 'DocumentReviewAndRevision
  }

  @combinator object DocumentReviewAndRevisionIteration {

    def apply(BuildingPermitNotApproved: String): String = "Document Review and Revision"
    //insert Atrributes and Graphcode Here
    //Duration = 25
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'BuildingPermitNotApproved =>: 'DocumentReviewAndRevision
  }

  @combinator object ApproveRevisedPlans{
    def apply(ReviewAndFinalizePlans: String,
              PrintConstructionDrawings:String):String = "ApproveRevisedPlans"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type ='ReviewAndFinalizePlans =>: 'PrintConstructionDrawings =>: 'ApproveRevisedPlans
  }


  @combinator object ApproveRevisedSpecifications{
    def apply(ReviewAndFinalizeSpecifications: String,
              PrintConstructionDrawings:String):String = "ApproveRevisedSpecifications"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'ReviewAndFinalizeSpecifications =>: 'PrintConstructionDrawings =>: 'ApproveRevisedSpecifications
  }

  @combinator object ApproveRevisedSitePlan{
    def apply(ReviewAndFinalizeSitePlan: String,
              PrintConstructionDrawings:String):String = "ApproveRevisedSitePlan"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'ReviewAndFinalizeSitePlan =>: 'PrintConstructionDrawings =>: 'ApproveRevisedSitePlan
  }

  @combinator object PrintConstructionDrawings{
    def  apply: String = "Print"

    //insert Atrributes and Graphcode Here
    //Duration = 5
    //Costs = 15431
    //Res = Builder
    val semanticType: Type = 'PrintConstructionDrawings
  }

  @combinator object ReviewAndFinalizePlans{
    def apply(SupplyContractPlans: String):String = "ReviewAndFinalizePlans"
    //insert Atrributes and Graphcode Here
    //Duration = 15
    //Costs = 24094
    //Res = Builder
    val semanticType: Type = 'SupplyContractPlans =>: 'ReviewAndFinalizePlans
  }

  @combinator object ReviewAndFinalizeSpecifications{
    def apply(SupplyContractPlans: String):String = "ReviewAndFinalizeSpecifications"
    //insert Atrributes and Graphcode Here
    //Duration = 20
    //Costs = 19599
    //Res = Builder
    val semanticType: Type = 'SupplyContractSpecifications =>: 'ReviewAndFinalizeSpecifications
  }

  @combinator object ReviewAndFinalizeSitePlan{
    def apply(SupplyContractPlans: String):String = "ReviewAndFinalizeSitePlan"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 22776
    //Res = Builder
    val semanticType: Type = 'SupplyContractSitePlan =>: 'ReviewAndFinalizeSitePlan
  }

  @combinator object ReviewAndFinalizePlansContract{
    def apply(Contracts: String):String = "ReviewAndFinalizePlans"
    //insert Atrributes and Graphcode Here
    //Duration = 15
    //Costs = 3124
    //Res = Builder
    val semanticType: Type = 'Contracts =>: 'ReviewAndFinalizePlans
  }

  @combinator object ReviewAndFinalizeSpecificationsContract{
    def apply(Contracts: String):String = "ReviewAndFinalizeSpecifications"
    //insert Atrributes and Graphcode Here
    //Duration = 20
    //Costs = 2363
    //Res = Builder
    val semanticType: Type = 'Contracts =>: 'ReviewAndFinalizeSpecifications
  }

  @combinator object ReviewAndFinalizeSitePlanContract{
    def apply(Contracts: String):String = "ReviewAndFinalizeSitePlan"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 19421
    //Res = Builder
    val semanticType: Type = 'Contracts =>: 'ReviewAndFinalizeSitePlan
  }

  @combinator object BidsAndContracts{
    def apply(Contracts: String,
              DocumentReviewAndRevision: String):String = "BidsAndContracts"
    //insert Atrributes and Graphcode Here
    //Duration = 24
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'Contracts =>: 'DocumentReviewAndRevision =>: 'BidsandContracts
  }

  @combinator object GradingAndBuildingPermits{
    def apply(BuildingPermitIssued: String):String = "GradingAndBuildingPermits"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'BuildingPermitIssued =>: 'GradingAndBuildingPermits
  }


  @combinator object BuildingPermitIssued{
    def apply(PayPermitFeesAndExciseTaxes: String):String = "BuildingPermitIssued"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'PayPermitFeesAndExciseTaxes =>: 'BuildingPermitIssued
  }

  @combinator object PayPermitFeesAndExciseTaxes{
    def apply(BuildingPermitApproved: String):String = "PayPermitFeesAndExciseTaxes"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 23598
    //Res = Builder
    val semanticType: Type = 'BuildingPermitApproved =>: 'BuildingPermitIssued
  }

  @combinator object BuildingPermitApproved{
    def apply(CountyPermitProcess:String,
              GradingPermitIssued: String):String = "BuildingPermitApproved"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 4904
    //Res = Department of Permits and Licenses
    val semanticType: Type = 'CountyPermitProcess =>: 'GradingPermitIssued =>: 'BuildingPermitApproved
  }

  @combinator object BuildingPermitNotApproved{
    def apply(CountyPermitProcess:String,
              GradingPermitIssued: String):String = "BuildingPermitNotApproved"
    //insert Atrributes and Graphcode Here
    //Duration = 12
    //Costs = 0
    //Res = Department of Permits and Licenses
    val semanticType: Type = 'CountyPermitProcess =>: 'GradingPermitIssued =>: 'BuildingPermitNotApproved
  }

  @combinator object GradingPermitIssued{
    def apply(SedimentControlInsp:String):String = "GradingPermitIssued"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 16246
    //Res = Sediment Control Inspector
    val semanticType: Type = 'SedimentControlInsp =>: 'GradingPermitIssued
  }

  @combinator object SedimentControlInsp{
    def apply(InstallConstructionEntrance:String,
              InstallSedimentControls: String):String = "SedimentControlInsp"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 6896
    //Res = Sediment Control Inspector
    val semanticType: Type = 'InstallConstructionEntrance =>: 'InstallSedimentControls =>: 'SedimentControlInsp
  }

  @combinator object InstallSedimentControls{
    def apply(MeetSedimentControlInspector:String,
              WalkLot: String):String = "InstallSedimentControls"
    //insert Atrributes and Graphcode Here
    //Duration = 2
    //Costs = 20680
    //Res = Excavation Subcontractor
    val semanticType: Type = 'InstallConstructionEntrance =>: 'WalkLot =>: 'InstallSedimentControls
  }

  @combinator object InstallConstructionEntrance{
    def apply(MeetSedimentControlInspector:String,
              WalkLot: String):String = "InstallConstructionEntrance"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 21520
    //Res = Excavation Subcontractor
    val semanticType: Type = 'MeetSedimentControlInspector =>: 'WalkLot =>: 'InstallConstructionEntrance
  }

  @combinator object WalkLot{
    def apply(MeetSedimentControlInspector:String):String = "WalkLot"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 13679
    //Res = Builder
    val semanticType: Type = 'MeetSedimentControlInspector =>: 'WalkLot
  }

  @combinator object MeetSedimentControlInspector{
    def apply(FileGradingPermitApplication:String,
              StakeLot:String,
              PostLotIdentification:String):String = "MeetSedimentControlInspector"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 9562
    //Res = Builder
    val semanticType: Type = 'FileGradingPermitApplication =>: 'StakeLot =>: 'PostLotIdentification =>: 'MeetSedimentControlInspector
  }

  @combinator object PostLotIdentification{
    def apply(FileBuildingPermitApplication:String,
              StakeLot: String):String = "PostLotIdentification"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 23422
    //Res = Builder
    val semanticType: Type = 'FileBuildingPermitApplication =>: 'StakeLot =>: 'PostLotIdentification
  }

  @combinator object StakeLot{
    def apply(ScheduleLotStakeOut:String):String = "StakeLot"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 19267
    //Res = Civil Engineer
    val semanticType: Type = 'ScheduleLotStakeOut =>: 'StakeLot
  }

  @combinator object CountyPermitProcess{
    def apply(FileBuildingPermitApplication:String):String = "CountyPermitProcess"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 1323
    //Res = Department of Permits and Licenses
    val semanticType: Type = 'FileBuildingPermitApplication =>: 'CountyPermitProcess
  }

  @combinator object FileBuildingPermitApplication{
    def apply(BidsandContracts:String):String = "FileBuildingPermitApplication"
    //insert Atrributes and Graphcode Here
    //Duration = 3
    //Costs = 22460
    //Res = Builder
    val semanticType: Type = 'BidsandContracts =>: 'FileBuildingPermitApplication
  }

  @combinator object FileGradingPermitApplication{
    def apply(BidsandContracts:String):String = "FileGradingPermitApplication"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 21813
    //Res = Builder
    val semanticType: Type = 'BidsandContracts =>: 'FileGradingPermitApplication
  }

  @combinator object ScheduleLotStakeOut{
    def apply(BidsandContracts:String):String = "ScheduleLotStakeOut"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 19257
    //Res = Builder
    val semanticType: Type = 'BidsandContracts =>: 'ScheduleLotStakeOut
  }

  @combinator object Foundation{
    def apply(StripTopsoilAndStockpile:String,
              FoundationCertification: String,
              SetLintelsBoltsCapBlock:String,
              WaterproofingAndDrainTile:String):String = "Foundation"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'StripTopsoilAndStockpile =>: 'FoundationCertification =>: 'SetLintelsBoltsCapBlock =>: 'WaterproofingAndDrainTile =>: 'Foundation
  }

  @combinator object StripTopsoilAndStockpile{
    def apply(ClearLot:String):String = "StripTopsoilAndStockpile"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 24089
    //Res = Excavation Subcontractor
    val semanticType: Type = 'ClearLot =>: 'StripTopsoilAndStockpile
  }

  @combinator object ClearLot{
    def apply(GradingAndBuildingPermits:String):String = "ClearLot"
    //insert Atrributes and Graphcode Here
    //Duration = 3
    //Costs = 2507
    //Res = Excavation Subcontractor
    val semanticType: Type = 'GradingAndBuildingPermits =>: 'ClearLot
  }

  @combinator object FoundationCertificationPrime{
    def apply(BuildBlockFoundation:String):String = "FoundationCertification"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'BuildBlockFoundation =>: 'FoundationCertification
  }

  @combinator object FoundationCertificationOption{
    def apply(FillBlockCoresWConcrete:String):String = "FoundationCertification"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'FillBlockCoresWConcrete =>: 'FoundationCertification
  }

  @combinator object SetLintelsBoltsCapBlock{
    def apply(SteelDelivery:String):String = "SetLintelsBoltsCapBlock"
    //insert Atrributes and Graphcode Here
    //Duration = 2
    //Costs = 14255
    //Res = Block Masonry Subcontractor
    val semanticType: Type = 'SteelDelivery =>: 'SetLintelsBoltsCapBlock
  }

  @combinator object SteelDelivery{
    def apply(FillBlockCoresWConcrete:String):String = "SteelDelivery"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 19770
    //Res = Steel Supplier
    val semanticType: Type = 'FillBlockCoresWConcrete =>: 'SteelDelivery
  }

  @combinator object LumberDelivery{
    def apply(FillBlockCoresWConcrete:String):String = "LumberDelivery"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 14271
    //Res = Lumber Supplier
    val semanticType: Type = 'FillBlockCoresWConcrete =>: 'LumberDelivery
  }

  @combinator object FillBlockCoresWConcrete{
    def apply(BuildBlockFoundation:String):String = "FillBlockCoresWConcrete"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 2052
    //Res = Block Masonry Subcontractor
    val semanticType: Type = 'BuildBlockFoundation =>: 'FillBlockCoresWConcrete
  }

  @combinator object WaterproofingAndDrainTile{
    def apply(LumberDelivery:String):String = "WaterproofingAndDrainTile"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 18062
    //Res = Waterproofing Subcontractor
    val semanticType: Type = 'LumberDelivery =>: 'WaterproofingAndDrainTile
  }

  @combinator object BuildBlockFoundation{
    def apply(PinFootings:String,
              StockBlockMortarSand:String):String = "BuildBlockFoundation"
    //insert Atrributes and Graphcode Here
    //Duration = 15
    //Costs = 16847
    //Res = Black Masonry Subcontractor
    val semanticType: Type = 'PinFootings =>: 'StockBlockMortarSand =>: 'BuildBlockFoundation
  }

  @combinator object StockBlockMortarSand{
    def apply(PinFootings:String):String = "StockBlockMortarSand"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 13424
    //Res = Black Masonry Subcontractor
    val semanticType: Type = 'PinFootings =>: 'StockBlockMortarSand
  }

  @combinator object PinFootings{
    def apply(PourFootings:String):String = "PinFootings"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 19029
    //Res = Civil Engineer
    val semanticType: Type = 'PourFootings =>: 'PinFootings
  }

  @combinator object PourFootings{
    def apply(FootingInspection:String):String = "PourFootings"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 5012
    //Res = Concrete Subcontractor
    val semanticType: Type = 'FootingInspection =>: 'PourFootings
  }

  @combinator object FootingInspection{
    def apply(DigFootingsAndInstallReinforcing:String):String = "FootingInspection"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'DigFootingsAndInstallReinforcing =>: 'FootingInspection
  }

  @combinator object DigFootingsAndInstallReinforcing{
    def apply(LayoutFootings:String):String = "DigFootingsAndInstallReinforcing"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 2515
    //Res = Concrete Subcontractor
    val semanticType: Type = 'LayoutFootings =>: 'DigFootingsAndInstallReinforcing
  }

  @combinator object LayoutFootings{
    def apply(SiteWork:String):String = "LayoutFootings"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 5416
    //Res = Concrete Subcontractor
    val semanticType: Type = 'SiteWork =>: 'LayoutFootings
  }

  @combinator object ExcavateForFoundation{
    def apply(ClearLot:String,
              StakeLotForExcavation:String,
              RoughGradeLot:String):String = "ExcavateForFoundation"
    //insert Atrributes and Graphcode Here
    //Duration = 2
    //Costs = 23705
    //Res = Excavation Subcontractor
    val semanticType: Type = 'ClearLot =>: 'StakeLotForExcavation =>: 'RoughGradeLot =>: 'ExcavateForFoundation
  }

  @combinator object RoughGradeLot{
    def apply(ClearLot:String,
              StakeLotForExcavation:String):String = "RoughGradeLot"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 13470
    //Res = Excavation Subcontractor
    val semanticType: Type = 'ClearLot =>: 'StakeLotForExcavation =>: 'RoughGradeLot
  }

  @combinator object StakeLotForExcavation{
    def apply(ClearLot:String):String = "StakeLotForExcavation"
    //insert Atrributes and Graphcode Here
    //Duration = 1
    //Costs = 17371
    //Res = Civil Engineer
    val semanticType: Type = 'ClearLot =>: 'StakeLotForExcavation
  }

  @combinator object SiteWork{
    def apply(ExcavateForFoundation:String):String = "SiteWork"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'ExcavateForFoundationt =>: 'SiteWork
  }

  @combinator object FullProject{
    def apply(Contracts:String, DocumentReviewAndRevision: String, BidsAndContracts: String, GradingAndBuildingPermits: String, SiteWork: String, Foundation: String): String = "FullProject"
    //insert Atrributes and Graphcode Here
    //Duration = 0
    //Costs = 0
    //Res = Null
    val semanticType: Type = 'Contracts =>: 'DocumentReviewAndRevision =>: 'BidsAndContracts =>: 'GradingAndBuildingPermits =>: 'SiteWork =>: 'Foundation =>: 'FullProject
  }

}

