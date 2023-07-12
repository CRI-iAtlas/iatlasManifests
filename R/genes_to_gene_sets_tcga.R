genes_to_gene_sets_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "entrez" = "entrez_id",
      "hgnc" = "hgnc_id",
      "gene_id" = "id"
    )

  gene_sets <-
    synapse_csv_id_to_tbl(syn, "syn51034026") %>%
    dplyr::select(
      "gene_set_name" = "name",
      "gene_set_id" = "id"
    )


  immunomodulators <-
    synapse_feather_id_to_tbl(syn,  "syn23518460") %>%
    dplyr::select("entrez") %>%
    tidyr::drop_na() %>%
    dplyr::mutate("gene_set_name" = "immunomodulator")

  io_targets <-
    synapse_feather_id_to_tbl(syn,  "syn22151533") %>%
    dplyr::select("entrez" = "Entrez ID") %>%
    dplyr::mutate("entrez" = as.integer(.data$entrez)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate("gene_set_name" = "io_target")

  potential_immunomodulators <-
    synapse_feather_id_to_tbl(syn,  "syn22151532") %>%
    dplyr::select("entrez" = "Entrez ID") %>%
    tidyr::drop_na() %>%
    dplyr::mutate("gene_set_name" = "potential_immunomodulator")

  extracellular_network <-
    synapse_feather_id_to_tbl(syn,  "syn23518510") %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::select("entrez" = "value") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "entrez" = as.integer(.data$entrez),
      "gene_set_name" = "extracellular_network"
    ) %>%
    tidyr::drop_na()

  cellimage_network <-
    synapse_feather_id_to_tbl(syn,  "syn23518512") %>%
    dplyr::select("from", "to") %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::select("entrez" = "value") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "entrez" = as.integer(.data$entrez),
      "gene_set_name" = "cellimage_network"
    ) %>%
    tidyr::drop_na()

  immune_subtype_classifier <-
    synapse_csv_id_to_tbl(syn, "syn26066271") %>%
    dplyr::select("entrez" = "Entrez") %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      "entrez" = as.integer(.data$entrez),
      "gene_set_name" = "immune_subtype_classifier_gene"
    )


  wolf <-
    synapse_tsv_id_to_tbl(syn, "syn22240714" ) %>%
    dplyr::select("entrez" = "Genes", "gene_set_name" = "GeneSet") %>%
    tidyr::drop_na() %>%
    dplyr::mutate("entrez" = as.integer(.data$entrez))

  yasin <-
    synapse_tsv_id_to_tbl(syn,  "syn22240715") %>%
    dplyr::select("entrez" = "Entrez", "gene_set_name" = "GeneSet") %>%
    tidyr::drop_na() %>%
    dplyr::mutate("entrez" = as.integer(.data$entrez))

  cibersort_gene_string <- "ABCB4,ABCB9,ACAP1,ACHE,ACP5,ADAM28,ADAMDEC1,ADAMTS3,ADRB2,AIF1,AIM2,ALOX15,ALOX5,AMPD1,ANGPT4,ANKRD55,APOBEC3A,APOBEC3G,APOL3,APOL6,AQP9,ARHGAP22,ARRB1,ASGR1,ASGR2,ATHL1,ATP8B4,ATXN8OS,AZU1,BACH2,BANK1,BARX2,BCL11B,BCL2A1,BCL7A,BEND5,BFSP1,BHLHE41,BIRC3,BLK,BMP2K,BPI,BRAF,BRSK2,BST1,BTNL8,C11orf80,C1orf54,C3AR1,C5AR1,C5AR2,CA8,CAMP,CASP5,CCDC102B,CCL1,CCL13,CCL14,CCL17,CCL18,CCL19,CCL20,CCL22,CCL23,CCL4,CCL5,CCL7,CCL8,CCND2,CCR10,CCR2,CCR3,CCR5,CCR6,CCR7,CD160,CD180,CD19,CD1A,CD1B,CD1C,CD1D,CD1E,CD2,CD209,CD22,CD244,CD247,CD27,CD28,CD300A,CD33,CD37,CD38,CD3D,CD3E,CD3G,CD4,CD40,CD40LG,CD5,CD6,CD68,CD69,CD7,CD70,CD72,CD79A,CD79B,CD80,CD86,CD8A,CD8B,CD96,CDA,CDC25A,CDH12,CDHR1,CDK6,CEACAM3,CEACAM8,CEMP1,CFP,CHI3L1,CHI3L2,CHST15,CHST7,CLC,CLCA3P,CLEC10A,CLEC2D,CLEC4A,CLEC7A,CLIC2,CMA1,COL8A2,COLQ,CPA3,CR2,CREB5,CRISP3,CRTAM,CRYBB1,CSF1,CSF2,CSF3R,CST7,CTLA4,CTSG,CTSW,CXCL10,CXCL11,CXCL13,CXCL3,CXCL5,CXCL9,CXCR1,CXCR2,CXCR5,CXCR6,CXorf57,CYP27A1,CYP27B1,DACH1,DAPK2,DCSTAMP,DEFA4,DENND5B,DEPDC5,DGKA,DHRS11,DHX58,DPEP2,DPP4,DSC1,DUSP2,EAF2,EBI3,EFNA5,EGR2,ELANE,EMR1,EMR2,EMR3,EPB41,EPHA1,EPN2,ETS1,ETV3,FAIM3,FAM124B,FAM174B,FAM198B,FAM212B,FAM65B,FASLG,FBXL8,FCER1A,FCER2,FCGR2B,FCGR3B,FCN1,FCRL2,FES,FFAR2,FLJ13197,FLT3LG,FLVCR2,FOSB,FOXP3,FPR1,FPR2,FPR3,FRK,FRMD4A,FRMD8,FZD2,FZD3,GAL3ST4,GALR1,GFI1,GGT5,GIPR,GNG7,GNLY,GPC4,GPR1,GPR171,GPR18,GPR183,GPR19,GPR25,GPR65,GPR97,GRAP2,GSTT1,GUSBP11,GYPE,GZMA,GZMB,GZMH,GZMK,GZMM,HAL,HCK,HDC,HESX1,HHEX,HIC1,HIST1H2AE,HIST1H2BG,HK3,HLA-DOB,HLA-DQA1,HMGB3P30,HNMT,HOXA1,HPGDS,HPSE,HRH1,HSPA6,HTR2B,ICA1,ICOS,IDO1,IFI44L,IFNA10,IFNG,IGHD,IGHE,IGHM,IGKC,IGLL3P,IGSF6,IL12B,IL12RB2,IL17A,IL18R1,IL18RAP,IL1A,IL1B,IL1RL1,IL21,IL26,IL2RA,IL2RB,IL3,IL4,IL4R,IL5,IL5RA,IL7,IL7R,IL9,IRF8,ITK,KCNA3,KCNG2,KIAA0226L,KIAA0754,KIR2DL1,KIR2DL4,KIR2DS4,KIR3DL2,KIRREL,KLRB1,KLRC3,KLRC4,KLRD1,KLRF1,KLRG1,KLRK1,KRT18P50,KYNU,LAG3,LAIR2,LAMP3,LAT,LCK,LEF1,LHCGR,LILRA2,LILRA3,LILRA4,LILRB2,LIME1,LINC00597,LINC00921,LOC100130100,LOC126987,LRMP,LST1,LTA,LTB,LTC4S,LY86,LY9,MAGEA11,MAK,MAN1A1,MANEA,MAP3K13,MAP4K1,MAP4K2,MAP9,MARCH3,MARCO,MAST1,MBL2,MEFV,MEP1A,MGAM,MICAL3,MMP12,MMP25,MMP9,MNDA,MROH7,MS4A1,MS4A2,MS4A3,MS4A6A,MSC,MXD1,MYB,MZB1,NAALADL1,NCF2,NCR3,NFE2,NIPSNAP3B,NKG7,NLRP3,NMBR,NME8,NOD2,NOX3,NPAS1,NPIPB15,NPL,NR4A3,NTN3,NTRK1,ORC1,OSM,P2RX1,P2RX5,P2RY10,P2RY13,P2RY14,P2RY2,PADI4,PAQR5,PASK,PAX7,PBXIP1,PCDHA5,PDCD1,PDCD1LG2,PDE6C,PDK1,PGLYRP1,PIK3IP1,PKD2L2,PLA1A,PLA2G7,PLCH2,PLEKHF1,PLEKHG3,PMCH,PNOC,PPBP,PPFIBP1,PRF1,PRG2,PRR5L,PSG2,PTGDR,PTGER2,PTGIR,PTPRCAP,PTPRG,PVRIG,QPCT,RAB27B,RALGPS2,RASA3,RASGRP2,RASGRP3,RASSF4,RCAN3,REN,RENBP,REPS2,RGS1,RGS13,RNASE2,RNASE6,RPL10L,RPL3P7,RRP12,RRP9,RSAD2,RYR1,S100A12,S1PR5,SAMSN1,SCN9A,SEC31B,SELL,SEPT5,SEPT8,SERGEF,SH2D1A,SIGLEC1,SIK1,SIRPG,SIT1,SKA1,SKAP1,SLAMF1,SLAMF8,SLC12A1,SLC12A8,SLC15A3,SLC2A6,SLC7A10,SLCO5A1,SMPD3,SMPDL3B,SOCS1,SP140,SPAG4,SPIB,SPOCK2,SSX1,ST3GAL6,ST6GALNAC4,ST8SIA1,STAP1,STEAP4,STXBP6,TARDBPP1,TBX21,TCF7,TCL1A,TEC,TEP1,TGM5,TLR2,TLR7,TLR8,TMEM156,TMEM255A,TNFAIP6,TNFRSF10C,TNFRSF11A,TNFRSF13B,TNFRSF17,TNFRSF4,TNFSF14,TNIP3,TPSAB1,TRAC,TRAF4,TRAT1,TRAV12-2,TRAV13-1,TRAV13-2,TRAV21,TRAV8-6,TRAV9-2,TRBC1,TRDC,TREM1,TREM2,TREML2,TRIB2,TRPM4,TRPM6,TSHR,TTC38,TXK,TYR,UBASH3A,UGT1A8,UGT2B17,UPK3A,VILL,VNN1,VNN2,VNN3,VPREB3,WNT5B,WNT7A,ZAP70,ZBP1,ZBTB10,ZBTB32,ZFP36L2,ZNF135,ZNF165,ZNF204P,ZNF222,ZNF286A,ZNF324,ZNF442"
  cibersort_genes <- cibersort_gene_string %>%
    stringr::str_split(",") %>%
    unlist() %>%
    dplyr::tibble("hgnc" = .) %>%
    dplyr::distinct() %>%
    dplyr::mutate("gene_set_name" = "cibersort_gene") %>%
    tidyr::drop_na()

  mcpcounter_genes <- "http://raw.githubusercontent.com/ebecht/MCPcounter/master/Signatures/genes.txt" %>%
    curl::curl() %>%
    readr::read_tsv() %>%
    dplyr::select("hgnc" = "HUGO symbols") %>%
    dplyr::mutate("gene_set_name" = "mcpcounter_gene") %>%
    tidyr::drop_na()

  epic_gene_string <- "PAX5,MS4A1,BANK1,CD79A,FCRL2,FCER2,TCL1A,CD79B,FCRL5,STAP1,POU2AF1,DPT,COL1A1,SFRP2,LUM,COL14A1,SFRP4,CXCL14,ISLR,COL3A1,MFAP5,ADAM33,MEG3,FBLN1,PTGIS,TMEM119,CRISPLD2,SYNPO2,F3,PRELP,CLDN11,FOXP3,ANKRD55,TRAT1,MDS2,IL2RA,GCNT4,RCAN3,TBC1D4,DGKA,CD8B,JAKMIP1,NAA16,TSPYL1,HAUS3,CDH5,VWF,CLEC14A,STAB2,SLCO2A1,CLDN5,MMRN1,MYCT1,GPR4,ECSCR,SOX18,KDR,CXorf36,MMRN2,GPR182,FLT4,PTPRB,F2RL3,FLT1,RHOJ,CD163,CSF1R,MSR1,FPR3,C1QC,SIGLEC1,VSIG4,CD14,IL1B,MS4A6A,APOC1,LILRB4,F13A1,CD300E,HAMP,CD300C,SH2D1B,KLRF1,NCR1,CLIC3,NMUR1,GNLY,CD160,S1PR5,FGFBP2,GNPTAB,UBASH3A,THEMIS,CD28,ITK,BCL11B,CD5,IL7R"
  epic_genes <- epic_gene_string %>%
    stringr::str_split(",") %>%
    unlist() %>%
    dplyr::tibble("hgnc" = .) %>%
    dplyr::distinct() %>%
    dplyr::mutate("gene_set_name" = "epic_gene") %>%
    tidyr::drop_na()

  tbl1 <-
    dplyr::bind_rows(wolf, yasin) %>%
    dplyr::mutate("gene_set_name" = stringr::str_replace_all(.data$gene_set_name, "[ \\.]", "_")) %>%
    dplyr::bind_rows(
      immunomodulators,
      io_targets,
      potential_immunomodulators,
      extracellular_network,
      cellimage_network,
      immune_subtype_classifier
    ) %>%
    dplyr::inner_join(genes, by = "entrez") %>%
    dplyr::select("gene_set_name", "gene_id")

  tbl2 <-
    dplyr::bind_rows(cibersort_genes, mcpcounter_genes, epic_genes) %>%
    dplyr::inner_join(genes, by = "hgnc") %>%
    dplyr::group_by(.data$hgnc) %>%
    dplyr::arrange(.data$entrez) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select("gene_set_name", "gene_id")

   genes_to_gene_sets <-
     dplyr::bind_rows(tbl1, tbl2) %>%
     dplyr::inner_join(gene_sets, by = "gene_set_name") %>%
     dplyr::select(-"gene_set_name") %>%
     dplyr::mutate(
       "id" = uuid::UUIDgenerate(n = dplyr::n()),
       "Component" = "genes_to_gene_sets"
     )

   synapse_store_table_as_csv(
     syn,
     genes_to_gene_sets,
     "syn51400858",
     "genes_to_gene_sets"
   )

}

