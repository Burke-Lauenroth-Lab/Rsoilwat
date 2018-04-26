/*
 * SW_R_lib.c
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/SW_Defines.h"

#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/SW_Carbon.h"
#include "SOILWAT2/SW_Output.h"

#include "rSW_Files.h"
#include "rSW_Model.h"
#include "rSW_Weather.h"
#include "rSW_Markov.h"
#include "rSW_Sky.h"
#include "rSW_VegProd.h"
#include "rSW_Site.h"
#include "rSW_VegEstab.h"
#include "rSW_Output.h"
#include "rSW_Carbon.h"
#include "rSW_SoilWater.h"
#include "rSW_Control.h"

#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

/* =================================================== */
/*                  Global Declarations                */
/* external by other routines elsewhere in the program */
/* --------------------------------------------------- */


SEXP Rlogfile;
SEXP InputData;
SEXP WeatherList;
Bool useFiles;
Bool collectInData;
Bool bWeatherList;

extern char _firstfile[1024];

extern SW_MODEL SW_Model;
extern SW_SOILWAT SW_Soilwat;
extern SW_VEGESTAB SW_VegEstab;


/* =================================================== */
/*                Module-Level Declarations            */
/* --------------------------------------------------- */


/**
 * Determines if a constant in the Parton equation 2.21 is invalid and would
 * thus cause extreme soil temperature values (see SW_Flow_lib.c ~1770)
 *
 * @param  none
 * @return an R boolean that denotes an error (TRUE) or lack of (FALSE)
 *
 */
SEXP tempError(void) {
	SEXP swR_temp_error;
	PROTECT(swR_temp_error = NEW_LOGICAL(1));
	LOGICAL_POINTER(swR_temp_error)[0] = SW_Soilwat.soiltempError;
	UNPROTECT(1);
	return swR_temp_error;
}


void initSOILWAT2(SEXP inputOptions) {
	int i, argc, debug = 0;
	char *argv[7];

	if (debug) swprintf("Set args\n");
	argc = length(inputOptions);
	if (argc > 7) {
		// fatal condition because argv is hard-coded to be of length 7; increase size of
		// argv if more command-line options are added to SOILWAT2 in the future
		sw_error(-1, "length(inputOptions) must be <= 7.");
	}
	for (i = 0; i < argc; i++) {
		argv[i] = (char *) CHAR(STRING_ELT(inputOptions, i));
	}

	if (debug) swprintf("Set call arguments\n");
	init_args(argc, argv);

  if (debug) swprintf("Initialize SOILWAT ...");
	SW_CTL_init_model(_firstfile);
	rSW_CTL_init_model2();
}


SEXP onGetInputDataFromFiles(SEXP inputOptions) {
	int debug = 0;
	SEXP swInputData, SW_DataList, swLog, oRlogfile;

	collectInData = TRUE;
	logged = FALSE;
	logfp = NULL;

	if (debug) swprintf("Set log\n");
	PROTECT(swLog = MAKE_CLASS("swLog"));
	PROTECT(oRlogfile = NEW_OBJECT(swLog));
	PROTECT(Rlogfile = GET_SLOT(oRlogfile,install("LogData")));

	initSOILWAT2(inputOptions);

	if (debug) swprintf("Read input from disk files\n");
	SW_CTL_read_inputs_from_disk();

  if (debug) swprintf("Copy data to classes\n");
	PROTECT(swInputData = MAKE_CLASS("swInputData"));
	PROTECT(SW_DataList = NEW_OBJECT(swInputData));
	SET_SLOT(SW_DataList, install("files"), onGet_SW_F());
	SET_SLOT(SW_DataList, install("years"), onGet_SW_MDL());
	SET_SLOT(SW_DataList, install("weather"), onGet_SW_WTH());
	SET_SLOT(SW_DataList, install("cloud"), onGet_SW_SKY());
	SET_SLOT(SW_DataList, install("cloud_daily"), onGet_SW_SKY_daily());
	SET_SLOT(SW_DataList, install("weatherHistory"), onGet_WTH_DATA());
	if (LOGICAL(GET_SLOT(GET_SLOT(SW_DataList, install("weather")), install("use_Markov")))[0]) {
		SET_SLOT(SW_DataList, install("markov"), onGet_MKV());
	}
	SET_SLOT(SW_DataList, install("prod"), onGet_SW_VPD());
	SET_SLOT(SW_DataList, install("site"), onGet_SW_SIT());
	SET_SLOT(SW_DataList, install("soils"), onGet_SW_LYR());
	SET_SLOT(SW_DataList, install("estab"), onGet_SW_VES());
	SET_SLOT(SW_DataList, install("carbon"), onGet_SW_CARBON());
	SET_SLOT(SW_DataList, install("output"), onGet_SW_OUT());
	SET_SLOT(SW_DataList, install("swc"), onGet_SW_SWC());
	SET_SLOT(SW_DataList, install("log"), oRlogfile);

	SW_SIT_clear_layers();
	SW_WTH_clear_runavg_list();
	SW_VES_clear();

	UNPROTECT(5);
	return SW_DataList;
}

SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList, SEXP quiet) {
	int debug = 0;
	SEXP outputData, swLog, oRlogfile;

	collectInData = FALSE;
	logged = FALSE;
	if (LOGICAL(coerceVector(quiet, LGLSXP))[0]) {
	  logfp = NULL; // so that 'LogError' knows that R should NOT print messages to the console
	} else {
	  logfp = stdin; // just any non-NULL FILE pointer so that 'LogError' knows that R should print messages to the console
	}

	if (isNull(inputData)) {
		useFiles = TRUE;
	} else {
		useFiles = FALSE;
		InputData = inputData;
	}

	//This is used to minimize copying weather data between similiar runs.
	if (isNull(weatherList)) {
		bWeatherList = FALSE;
	} else {
		bWeatherList = TRUE;
		WeatherList = weatherList;
	}

  if (debug) swprintf("'start': create log ...");
	PROTECT(swLog = MAKE_CLASS("swLog"));
	PROTECT(oRlogfile = NEW_OBJECT(swLog));
	PROTECT(Rlogfile = GET_SLOT(oRlogfile,install("LogData")));

  if (debug) swprintf(" input arguments ...");
	initSOILWAT2(inputOptions);

	if (debug) swprintf(" obtain inputs ...");
	//Obtain the input data either from files or from memory (depending on useFiles)
	rSW_CTL_obtain_inputs();

  if (debug) swprintf(" setup output variables ...");
	SW_OUT_set_ncol();
	SW_OUT_set_colnames();
	PROTECT(outputData = onGetOutput(inputData));
	setGlobalrSOILWAT2_OutputVariables(outputData);

  if (debug) swprintf(" run SOILWAT ...");
	SW_CTL_main();

  if (debug) swprintf(" clean up ...");
	SW_SIT_clear_layers();
	SW_WTH_clear_runavg_list();
	SW_VES_clear();
  if (debug) swprintf(" completed.\n");

	UNPROTECT(4);

	return(outputData);
}


/** Expose SOILWAT2 constants and defines to internal R code of rSOILWAT2
  @return A list with six elements: one element `kINT` for integer constants;
    other elements contain vegetation keys, `VegTypes`; output keys, `OutKeys`;
    output periods, `OutPeriods`; output aggregation types, `OutAggs`; and names of
    input files, `InFiles`.
 */
SEXP sw_consts(void) {
  const int nret = 6; // length of cret
  const int nINT = 9; // length of vINT and cINT

  SEXP ret, cnames, ret_int, ret_int2, ret_str1, ret_str2, ret_str3, ret_infiles;
  int i;
  int *pvINT;
  char *cret[] = {"kINT", "VegTypes", "OutKeys", "OutPeriods", "OutAggs", "InFiles"};

  int vINT[] = {SW_NFILES, MAX_LAYERS, MAX_TRANSP_REGIONS, MAX_NYEAR, SW_MISSING,
    SW_OUTNPERIODS, SW_OUTNKEYS, SW_NSUMTYPES, NVEGTYPES};
  char *cINT[] = {"SW_NFILES", "MAX_LAYERS", "MAX_TRANSP_REGIONS", "MAX_NYEAR", "SW_MISSING",
    "SW_OUTNPERIODS", "SW_OUTNKEYS", "SW_NSUMTYPES", "NVEGTYPES"};
  int vINT2[] = {SW_TREES, SW_SHRUB, SW_FORBS, SW_GRASS};
  char *cINT2[] = {"SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS"};

  char *vSTR1[] = { SW_WETHR, SW_TEMP, SW_PRECIP, SW_SOILINF, SW_RUNOFF, SW_ALLH2O, SW_VWCBULK,
			SW_VWCMATRIC, SW_SWCBULK, SW_SWABULK, SW_SWAMATRIC, SW_SWA, SW_SWPMATRIC,
			SW_SURFACEW, SW_TRANSP, SW_EVAPSOIL, SW_EVAPSURFACE, SW_INTERCEPTION,
			SW_LYRDRAIN, SW_HYDRED, SW_ET, SW_AET, SW_PET, SW_WETDAY, SW_SNOWPACK,
			SW_DEEPSWC, SW_SOILTEMP,
			SW_ALLVEG, SW_ESTAB, SW_CO2EFFECTS };  // TODO: this is identical to SW_Output.c/key2str
  char *cSTR1[] = {"SW_WETHR", "SW_TEMP", "SW_PRECIP", "SW_SOILINF", "SW_RUNOFF",
    "SW_ALLH2O", "SW_VWCBULK", "SW_VWCMATRIC", "SW_SWCBULK", "SW_SWABULK",
    "SW_SWAMATRIC", "SW_SWA", "SW_SWPMATRIC", "SW_SURFACEW", "SW_TRANSP", "SW_EVAPSOIL",
    "SW_EVAPSURFACE", "SW_INTERCEPTION", "SW_LYRDRAIN", "SW_HYDRED", "SW_ET", "SW_AET",
    "SW_PET", "SW_WETDAY", "SW_SNOWPACK", "SW_DEEPSWC", "SW_SOILTEMP", "SW_ALLVEG",
    "SW_ESTAB", "SW_CO2EFFECTS"};
  char *vSTR2[] = {SW_DAY, SW_WEEK, SW_MONTH, SW_YEAR}; // TODO: this is identical to SW_Output.c/pd2str
  char *cSTR2[] = {"SW_DAY", "SW_WEEK", "SW_MONTH", "SW_YEAR"};
  char *vSTR3[] = {SW_SUM_OFF, SW_SUM_SUM, SW_SUM_AVG, SW_SUM_FNL}; // TODO: this is identical to SW_Output.c/styp2str
  char *cSTR3[] = {"SW_SUM_OFF", "SW_SUM_SUM", "SW_SUM_AVG", "SW_SUM_FNL"};
  char *cInF[] = {"eFirst", "eModel", "eLog", "eSite", "eLayers", "eWeather",
    "eMarkovProb",  "eMarkovCov", "eSky", "eVegProd", "eVegEstab", "eCarbon", "eSoilwat",
    "eOutput", "eOutputDaily","eOutputWeekly","eOutputMonthly","eOutputYearly",
		"eOutputDaily_soil","eOutputWeekly_soil","eOutputMonthly_soil","eOutputYearly_soil"}; // TODO: this must match SW_Files.h/SW_FileIndex

  // create vector of integer constants
  PROTECT(ret_int = allocVector(INTSXP, nINT));
  pvINT = INTEGER(ret_int);
  PROTECT(cnames = allocVector(STRSXP, nINT));
  for (i = 0; i < nINT; i++) {
    pvINT[i] = vINT[i];
    SET_STRING_ELT(cnames, i, mkChar(cINT[i]));
  }
  namesgets(ret_int, cnames);

  // create vector of vegetation types
  PROTECT(ret_int2 = allocVector(INTSXP, NVEGTYPES));
  pvINT = INTEGER(ret_int2);
  PROTECT(cnames = allocVector(STRSXP, NVEGTYPES));
  for (i = 0; i < NVEGTYPES; i++) {
    pvINT[i] = vINT2[i];
    SET_STRING_ELT(cnames, i, mkChar(cINT2[i]));
  }
  namesgets(ret_int2, cnames);

  // create vector of output key constants
  PROTECT(ret_str1 = allocVector(STRSXP, SW_OUTNKEYS));
  PROTECT(cnames = allocVector(STRSXP, SW_OUTNKEYS));
  for (i = 0; i < SW_OUTNKEYS; i++) {
    SET_STRING_ELT(ret_str1, i, mkChar(vSTR1[i]));
    SET_STRING_ELT(cnames, i, mkChar(cSTR1[i]));
  }
  namesgets(ret_str1, cnames);

  // create vector of output period constants
  PROTECT(ret_str2 = allocVector(STRSXP, SW_OUTNPERIODS));
  PROTECT(cnames = allocVector(STRSXP, SW_OUTNPERIODS));
  for (i = 0; i < SW_OUTNPERIODS; i++) {
    SET_STRING_ELT(ret_str2, i, mkChar(vSTR2[i]));
    SET_STRING_ELT(cnames, i, mkChar(cSTR2[i]));
  }
  namesgets(ret_str2, cnames);

  // create vector of output summary constants
  PROTECT(ret_str3 = allocVector(STRSXP, SW_NSUMTYPES));
  PROTECT(cnames = allocVector(STRSXP, SW_NSUMTYPES));
  for (i = 0; i < SW_NSUMTYPES; i++) {
    SET_STRING_ELT(ret_str3, i, mkChar(vSTR3[i]));
    SET_STRING_ELT(cnames, i, mkChar(cSTR3[i]));
  }
  namesgets(ret_str3, cnames);

  // create vector of input file descriptors
  PROTECT(ret_infiles = allocVector(INTSXP, SW_NFILES));
  pvINT = INTEGER(ret_infiles);
  PROTECT(cnames = allocVector(STRSXP, SW_NFILES));
  for (i = 0; i < SW_NFILES; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(cInF[i]));
  }
  namesgets(ret_infiles, cnames);


  // combine vectors into a list and return
  PROTECT(ret = allocVector(VECSXP, nret));
  PROTECT(cnames = allocVector(STRSXP, nret));
  for (i = 0; i < nret; i++)
    SET_STRING_ELT(cnames, i, mkChar(cret[i]));
  namesgets(ret, cnames);
  SET_VECTOR_ELT(ret, 0, ret_int);
  SET_VECTOR_ELT(ret, 1, ret_int2);
  SET_VECTOR_ELT(ret, 2, ret_str1);
  SET_VECTOR_ELT(ret, 3, ret_str2);
  SET_VECTOR_ELT(ret, 4, ret_str3);
  SET_VECTOR_ELT(ret, 5, ret_infiles);

  UNPROTECT(14);

  return ret;
}
