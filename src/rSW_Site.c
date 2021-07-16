/********************************************************/
/********************************************************/
/*	Application: SOILWAT - soilwater dynamics simulator
 Source file: Site.c
 Type: module
 Purpose: Read / write and otherwise manage the
 site specific information.  See also the
 Layer module.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/myMemory.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/SW_SoilWater.h"

#include "SOILWAT2/SW_Site.h"
#include "rSW_Site.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */


extern SEXP InputData;
extern SW_SITE SW_Site;
extern Bool EchoInits;

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

/* transpiration regions  shallow, moderately shallow,  */
/* deep and very deep. units are in layer numbers. */
extern LyrIndex _TranspRgnBounds[MAX_TRANSP_REGIONS];

/* for these three, units are cm/cm if < 1, -bars if >= 1 */
extern RealD _SWCInitVal, /* initialization value for swc */
  _SWCWetVal, /* value for a "wet" day,       */
  _SWCMinVal; /* lower bound on swc.          */

char *cSW_SIT[] = { "SWClimits", "ModelFlags", "ModelCoefficients",
  "SnowSimulationParameters", "DrainageCoefficient", "EvaporationCoefficients",
  "TranspirationCoefficients", "IntrinsicSiteParams", "SoilTemperatureFlag",
  "SoilTemperatureConstants", "TranspirationRegions" };
char *cLayers[] = { "depth_cm", "bulkDensity_g/cm^3", "gravel_content",
  "EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac", "transpTree_frac",
  "transpForb_frac", "sand_frac", "clay_frac", "impermeability_frac", "soilTemp_c" };


/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */


SEXP onGet_SW_LYR() {
	int i, dmax = 0;
	SW_SITE *v = &SW_Site;
	SEXP swSoils, SW_SOILS;
	SEXP Layers,Layers_names,Layers_names_y;
	RealD *p_Layers;

	PROTECT(swSoils = MAKE_CLASS("swSoils"));
	PROTECT(SW_SOILS = NEW_OBJECT(swSoils));
	PROTECT(Layers = allocMatrix(REALSXP,v->n_layers,12));
	p_Layers = REAL(Layers);
	for (i = 0; i < (v->n_layers); i++) {
		p_Layers[i + (v->n_layers) * 0] = dmax = v->lyr[i]->width + dmax;
		p_Layers[i + (v->n_layers) * 1] = v->lyr[i]->soilMatric_density;
		p_Layers[i + (v->n_layers) * 2] = v->lyr[i]->fractionVolBulk_gravel;
		p_Layers[i + (v->n_layers) * 3] = v->lyr[i]->evap_coeff;
		p_Layers[i + (v->n_layers) * 4] = v->lyr[i]->transp_coeff[SW_GRASS];
		p_Layers[i + (v->n_layers) * 5] = v->lyr[i]->transp_coeff[SW_SHRUB];
		p_Layers[i + (v->n_layers) * 6] = v->lyr[i]->transp_coeff[SW_TREES];
		p_Layers[i + (v->n_layers) * 7] = v->lyr[i]->transp_coeff[SW_FORBS];
		p_Layers[i + (v->n_layers) * 8] = v->lyr[i]->fractionWeightMatric_sand;
		p_Layers[i + (v->n_layers) * 9] = v->lyr[i]->fractionWeightMatric_clay;
		p_Layers[i + (v->n_layers) * 10] = v->lyr[i]->impermeability;
		p_Layers[i + (v->n_layers) * 11] = v->lyr[i]->sTemp;
	}
	PROTECT(Layers_names = allocVector(VECSXP,2));
	PROTECT(Layers_names_y = allocVector(STRSXP,12));
	for(i=0;i<12;i++)
		SET_STRING_ELT(Layers_names_y,i,mkChar(cLayers[i]));
	SET_VECTOR_ELT(Layers_names,1,Layers_names_y);
	setAttrib(Layers, R_DimNamesSymbol, Layers_names);

	SET_SLOT(SW_SOILS,install("Layers"),Layers);

	UNPROTECT(5);
	return SW_SOILS;
}

/* Function `onSet_SW_LYR()` corresponds to SOILWAT2's `_read_layers()` */
void onSet_SW_LYR(SEXP SW_SOILS) {

	SW_SITE *v = &SW_Site;
	LyrIndex lyrno;
	int i, j, k, columns;
	RealF dmin = 0.0, dmax, evco, trco_veg[NVEGTYPES], psand, pclay, matricd, imperm, soiltemp, f_gravel;
	RealD *p_Layers;
	SEXP SW_LYR;

	/* note that Files.read() must be called prior to this. */
	PROTECT(SW_LYR = GET_SLOT(SW_SOILS,install("Layers")));
	MyFileName = SW_F_name(eLayers);


	j = nrows(SW_LYR);
	p_Layers = REAL(SW_LYR);
	columns = ncols(SW_LYR);

	/* Check that we have 12 values per layer */
	/* Adjust number if new variables are added */
	if (columns != 12) {
		LogError(
			logfp,
			LOGFATAL,
			"%s : Too few columns in layers specified (%d).\n",
			MyFileName, columns
		);
	}

	for (i = 0; i < j; i++) {
		lyrno = _newlayer();

		dmax = p_Layers[i + j * 0];
		matricd = p_Layers[i + j * 1];
		f_gravel = p_Layers[i + j * 2];
		evco = p_Layers[i + j * 3];
		trco_veg[SW_GRASS] = p_Layers[i + j * 4];
		trco_veg[SW_SHRUB] = p_Layers[i + j * 5];
		trco_veg[SW_TREES] = p_Layers[i + j * 6];
		trco_veg[SW_FORBS] = p_Layers[i + j * 7];
		psand = p_Layers[i + j * 8];
		pclay = p_Layers[i + j * 9];
		imperm = p_Layers[i + j * 10];
		soiltemp = p_Layers[i + j * 11];

		v->lyr[lyrno]->width = dmax - dmin;
		dmin = dmax;
		v->lyr[lyrno]->soilMatric_density = matricd;
		v->lyr[lyrno]->fractionVolBulk_gravel = f_gravel;
		v->lyr[lyrno]->evap_coeff = evco;
		ForEachVegType(k) {
			v->lyr[lyrno]->transp_coeff[k] = trco_veg[k];
		}
		v->lyr[lyrno]->fractionWeightMatric_sand = psand;
		v->lyr[lyrno]->fractionWeightMatric_clay = pclay;
		v->lyr[lyrno]->impermeability = imperm;
		v->lyr[lyrno]->sTemp = soiltemp;

		if (lyrno >= MAX_LAYERS) {
			LogError(
				logfp,
				LOGFATAL,
				"%s : Too many layers specified (%d).\n"
				"Maximum number of layers is %d\n",
				MyFileName, lyrno + 1, MAX_LAYERS
			);
		}

	}

	UNPROTECT(1);
}

SEXP onGet_SW_SIT() {
	int i;
	SW_SITE *v = &SW_Site;

	SEXP swSite;
	SEXP SW_SIT;

	SEXP SWClimits, SWClimits_names;
	char *cSWClimits[] = { "swc_min", "swc_init", "swc_wet" };

	SEXP ModelFlags, ModelCoefficients, ModelFlags_names, ModelCoefficients_names;
	char *cModelFlags[] = { "Reset", "DeepDrain" };
	char *cModelCoefficients[] = {"PETmultiplier", "DailyRunoff", "DailyRunon" };

	SEXP SnowSimulationParameters, SnowSimulationParameters_names;
	char *cSnowSimulationParameters[] = { "TminAccu2", "TmaxCrit", "lambdaSnow", "RmeltMin", "RmeltMax" };

	SEXP DrainageCoefficient, DrainageCoefficient_names;

	SEXP EvaporationCoefficients, EvaporationCoefficients_names;
	char *cEvaporationCoefficients[] = { "RateShift", "RateSlope", "InflectionPoint", "Range" };

	SEXP TranspirationCoefficients, TranspirationCoefficients_names;
	char *cTranspirationCoefficients[] = { "RateShift", "RateShape", "InflectionPoint", "Range" };

	SEXP IntrinsicSiteParams, IntrinsicSiteParams_names;
	char *cIntrinsicSiteParams[] = { "Longitude", "Latitude", "Altitude", "Slope", "Aspect" };

	SEXP SoilTemperatureConstants_use, SoilTemperatureConstants, SoilTemperatureConstants_names;
	char *cSoilTempValues[] = { "BiomassLimiter_g/m^2", "T1constant_a", "T1constant_b", "T1constant_c", "cs_constant_SoilThermCondct", "cs_constant", "sh_constant_SpecificHeatCapacity",
			"ConstMeanAirTemp", "deltaX_Param", "MaxDepth" };

	SEXP TranspirationRegions, TranspirationRegions_names, TranspirationRegions_names_y;
	char *cTranspirationRegions[] = { "ndx", "layer" };
	int *p_transp; // ideally `LyrIndex` so that same type as `_TranspRgnBounds`, but R API INTEGER() is signed

	MyFileName = SW_F_name(eSite);

	PROTECT(swSite = MAKE_CLASS("swSite"));
	PROTECT(SW_SIT = NEW_OBJECT(swSite));

	PROTECT(SWClimits = allocVector(REALSXP, 3));
	REAL(SWClimits)[0] = _SWCMinVal;
	REAL(SWClimits)[1] = _SWCInitVal;
	REAL(SWClimits)[2] = _SWCWetVal;
	PROTECT(SWClimits_names = allocVector(STRSXP, 3));
	for (i = 0; i < 3; i++)
		SET_STRING_ELT(SWClimits_names, i, mkChar(cSWClimits[i]));
	setAttrib(SWClimits, R_NamesSymbol, SWClimits_names);

	PROTECT(ModelFlags = NEW_LOGICAL(2));
	LOGICAL(ModelFlags)[0] = v->reset_yr;
	LOGICAL(ModelFlags)[1] = v->deepdrain;
	PROTECT(ModelFlags_names = allocVector(STRSXP,2));
	for(i=0;i<2;i++)
		SET_STRING_ELT(ModelFlags_names,i,mkChar(cModelFlags[i]));
	setAttrib(ModelFlags, R_NamesSymbol, ModelFlags_names);

	PROTECT(ModelCoefficients = NEW_NUMERIC(3));
	REAL(ModelCoefficients)[0] = v->pet_scale;
	REAL(ModelCoefficients)[1] = v->percentRunoff;
	REAL(ModelCoefficients)[2] = v->percentRunon;
	PROTECT(ModelCoefficients_names = allocVector(STRSXP, 3));
	for (i = 0; i < 3; i++)
		SET_STRING_ELT(ModelCoefficients_names, i, mkChar(cModelCoefficients[i]));
	setAttrib(ModelCoefficients, R_NamesSymbol, ModelCoefficients_names);

	PROTECT(SnowSimulationParameters = allocVector(REALSXP, 5));
	REAL(SnowSimulationParameters)[0] = v->TminAccu2;
	REAL(SnowSimulationParameters)[1] = v->TmaxCrit;
	REAL(SnowSimulationParameters)[2] = v->lambdasnow;
	REAL(SnowSimulationParameters)[3] = v->RmeltMin;
	REAL(SnowSimulationParameters)[4] = v->RmeltMax;
	PROTECT(SnowSimulationParameters_names = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(SnowSimulationParameters_names, i, mkChar(cSnowSimulationParameters[i]));
	setAttrib(SnowSimulationParameters, R_NamesSymbol, SnowSimulationParameters_names);

	PROTECT(DrainageCoefficient = NEW_NUMERIC(1));
	REAL(DrainageCoefficient)[0] = v->slow_drain_coeff;
	PROTECT(DrainageCoefficient_names = allocVector(STRSXP,1));
	SET_STRING_ELT(DrainageCoefficient_names, 0, mkChar("SlowDrainCoefficientPerYear_cm/dy"));
	setAttrib(DrainageCoefficient, R_NamesSymbol, DrainageCoefficient_names);

	PROTECT(EvaporationCoefficients = allocVector(REALSXP,4));
	REAL(EvaporationCoefficients)[0] = v->evap.xinflec;
	REAL(EvaporationCoefficients)[1] = v->evap.slope;
	REAL(EvaporationCoefficients)[2] = v->evap.yinflec;
	REAL(EvaporationCoefficients)[3] = v->evap.range;
	PROTECT(EvaporationCoefficients_names = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(EvaporationCoefficients_names, i, mkChar(cEvaporationCoefficients[i]));
	setAttrib(EvaporationCoefficients, R_NamesSymbol, EvaporationCoefficients_names);

	PROTECT(TranspirationCoefficients = allocVector(REALSXP,4));
	REAL(TranspirationCoefficients)[0] = v->transp.xinflec;
	REAL(TranspirationCoefficients)[1] = v->transp.slope;
	REAL(TranspirationCoefficients)[2] = v->transp.yinflec;
	REAL(TranspirationCoefficients)[3] = v->transp.range;
	PROTECT(TranspirationCoefficients_names = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(TranspirationCoefficients_names, i, mkChar(cTranspirationCoefficients[i]));
	setAttrib(TranspirationCoefficients, R_NamesSymbol, TranspirationCoefficients_names);

	// SOILWAT2 calculates internally in radians, but input/output are in arc-degrees
	PROTECT(IntrinsicSiteParams = allocVector(REALSXP, 5));
	REAL(IntrinsicSiteParams)[0] = v->longitude * rad_to_deg;
	REAL(IntrinsicSiteParams)[1] = v->latitude * rad_to_deg;
	REAL(IntrinsicSiteParams)[2] = v->altitude;
	REAL(IntrinsicSiteParams)[3] = v->slope * rad_to_deg;
	REAL(IntrinsicSiteParams)[4] = missing(v->aspect) ? SW_MISSING : v->aspect * rad_to_deg;
	PROTECT(IntrinsicSiteParams_names = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(IntrinsicSiteParams_names, i, mkChar(cIntrinsicSiteParams[i]));
	setAttrib(IntrinsicSiteParams, R_NamesSymbol, IntrinsicSiteParams_names);

	PROTECT(SoilTemperatureConstants_use = NEW_LOGICAL(1));
	LOGICAL(SoilTemperatureConstants_use)[0] = v->use_soil_temp;

	PROTECT(SoilTemperatureConstants = NEW_NUMERIC(10));
	REAL(SoilTemperatureConstants)[0] = v->bmLimiter;
	REAL(SoilTemperatureConstants)[1] = v->t1Param1;
	REAL(SoilTemperatureConstants)[2] = v->t1Param2;
	REAL(SoilTemperatureConstants)[3] = v->t1Param3;
	REAL(SoilTemperatureConstants)[4] = v->csParam1;
	REAL(SoilTemperatureConstants)[5] = v->csParam2;
	REAL(SoilTemperatureConstants)[6] = v->shParam;
	REAL(SoilTemperatureConstants)[7] = v->Tsoil_constant;
	REAL(SoilTemperatureConstants)[8] = v->stDeltaX;
	REAL(SoilTemperatureConstants)[9] = v->stMaxDepth;
	PROTECT(SoilTemperatureConstants_names = allocVector(STRSXP,10));
	for (i = 0; i < 10; i++)
		SET_STRING_ELT(SoilTemperatureConstants_names, i, mkChar(cSoilTempValues[i]));
	setAttrib(SoilTemperatureConstants, R_NamesSymbol, SoilTemperatureConstants_names);

	PROTECT(TranspirationRegions = allocMatrix(INTSXP,(v->n_transp_rgn),2));
	p_transp = INTEGER(TranspirationRegions);
	for (i = 0; i < (v->n_transp_rgn); i++) {
		p_transp[i + (v->n_transp_rgn) * 0] = (i + 1);
		p_transp[i + (v->n_transp_rgn) * 1] = (_TranspRgnBounds[i]+1);
	}
	PROTECT(TranspirationRegions_names = allocVector(VECSXP,2));
	PROTECT(TranspirationRegions_names_y = allocVector(STRSXP,2));
	for (i = 0; i < 2; i++)
		SET_STRING_ELT(TranspirationRegions_names_y, i, mkChar(cTranspirationRegions[i]));
	SET_VECTOR_ELT(TranspirationRegions_names, 1, TranspirationRegions_names_y);
	setAttrib(TranspirationRegions, R_DimNamesSymbol, TranspirationRegions_names);

	SET_SLOT(SW_SIT, install(cSW_SIT[0]), SWClimits);
	SET_SLOT(SW_SIT, install(cSW_SIT[1]), ModelFlags);
	SET_SLOT(SW_SIT, install(cSW_SIT[2]), ModelCoefficients);
	SET_SLOT(SW_SIT, install(cSW_SIT[3]), SnowSimulationParameters);
	SET_SLOT(SW_SIT, install(cSW_SIT[4]), DrainageCoefficient);
	SET_SLOT(SW_SIT, install(cSW_SIT[5]), EvaporationCoefficients);
	SET_SLOT(SW_SIT, install(cSW_SIT[6]), TranspirationCoefficients);
	SET_SLOT(SW_SIT, install(cSW_SIT[7]), IntrinsicSiteParams);
	SET_SLOT(SW_SIT, install(cSW_SIT[8]), SoilTemperatureConstants_use);
	SET_SLOT(SW_SIT, install(cSW_SIT[9]), SoilTemperatureConstants);
	SET_SLOT(SW_SIT, install(cSW_SIT[10]), TranspirationRegions);

	UNPROTECT(24);
	return SW_SIT;
}

void onSet_SW_SIT(SEXP SW_SIT) {
	int i;
	SW_SITE *v = &SW_Site;

	SEXP SWClimits;
	SEXP ModelFlags;
	SEXP ModelCoefficients;
	SEXP SnowSimulationParameters;
	SEXP DrainageCoefficient;
	SEXP EvaporationCoefficients;
	SEXP TranspirationCoefficients;
	SEXP IntrinsicSiteParams;
	SEXP SoilTemperatureConstants_use;
	SEXP SoilTemperatureConstants;
	SEXP TranspirationRegions;
	int *p_transp; // ideally `LyrIndex` so that same type as `_TranspRgnBounds`, but R API INTEGER() is signed

  #ifdef RSWDEBUG
  int debug = 0;
  #endif

	MyFileName = SW_F_name(eSite);

	LyrIndex r; /* transp region definition number */
	Bool too_many_regions = FALSE;

	#ifdef RSWDEBUG
	if (debug) swprintf("'onSet_SW_SIT':");
	#endif

	PROTECT(SWClimits = GET_SLOT(SW_SIT, install("SWClimits")));
	_SWCMinVal = REAL(SWClimits)[0];
	_SWCInitVal = REAL(SWClimits)[1];
	_SWCWetVal = REAL(SWClimits)[2];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'SWClimits'");
	#endif

	PROTECT(ModelFlags = GET_SLOT(SW_SIT, install("ModelFlags")));
	v->reset_yr = LOGICAL(ModelFlags)[0];
	v->deepdrain = LOGICAL(ModelFlags)[1];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'flags'");
	#endif

	PROTECT(ModelCoefficients = GET_SLOT(SW_SIT, install("ModelCoefficients")));
	v->pet_scale = REAL(ModelCoefficients)[0];
	v->percentRunoff = REAL(ModelCoefficients)[1];
	v->percentRunon = REAL(ModelCoefficients)[2];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'coefs'");
	#endif

	PROTECT(SnowSimulationParameters = GET_SLOT(SW_SIT, install("SnowSimulationParameters")));
	v->TminAccu2 = REAL(SnowSimulationParameters)[0];
	v->TmaxCrit = REAL(SnowSimulationParameters)[1];
	v->lambdasnow = REAL(SnowSimulationParameters)[2];
	v->RmeltMin = REAL(SnowSimulationParameters)[3];
	v->RmeltMax = REAL(SnowSimulationParameters)[4];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'snow'");
	#endif

	PROTECT(DrainageCoefficient = GET_SLOT(SW_SIT, install("DrainageCoefficient")));
	v->slow_drain_coeff = REAL(DrainageCoefficient)[0];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'drain-coef'");
	#endif

	PROTECT(EvaporationCoefficients = GET_SLOT(SW_SIT, install("EvaporationCoefficients")));
	v->evap.xinflec = REAL(EvaporationCoefficients)[0];
	v->evap.slope = REAL(EvaporationCoefficients)[1];
	v->evap.yinflec = REAL(EvaporationCoefficients)[2];
	v->evap.range = REAL(EvaporationCoefficients)[3];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'evap-coef'");
	#endif

	PROTECT(TranspirationCoefficients = GET_SLOT(SW_SIT, install("TranspirationCoefficients")));
	v->transp.xinflec = REAL(TranspirationCoefficients)[0];
	v->transp.slope = REAL(TranspirationCoefficients)[1];
	v->transp.yinflec = REAL(TranspirationCoefficients)[2];
	v->transp.range = REAL(TranspirationCoefficients)[3];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'transp-coef'");
	#endif

	// SOILWAT2 calculates internally in radians, but input/output are in arc-degrees
	PROTECT(IntrinsicSiteParams = GET_SLOT(SW_SIT, install("IntrinsicSiteParams")));
	v->longitude = REAL(IntrinsicSiteParams)[0] * deg_to_rad;
	v->latitude = REAL(IntrinsicSiteParams)[1] * deg_to_rad;
	v->altitude = REAL(IntrinsicSiteParams)[2];
	v->slope = REAL(IntrinsicSiteParams)[3] * deg_to_rad;
	v->aspect = REAL(IntrinsicSiteParams)[4];
	v->aspect = missing(v->aspect) ? SW_MISSING : v->aspect * deg_to_rad;
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'location'");
	#endif

	PROTECT(SoilTemperatureConstants_use = GET_SLOT(SW_SIT, install("SoilTemperatureFlag")));
	v->use_soil_temp = LOGICAL(SoilTemperatureConstants_use)[0];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'soiltemp-flag'");
	#endif

	PROTECT(SoilTemperatureConstants = GET_SLOT(SW_SIT, install("SoilTemperatureConstants")));
	v->bmLimiter = REAL(SoilTemperatureConstants)[0];
	v->t1Param1 = REAL(SoilTemperatureConstants)[1];
	v->t1Param2 = REAL(SoilTemperatureConstants)[2];
	v->t1Param3 = REAL(SoilTemperatureConstants)[3];
	v->csParam1 = REAL(SoilTemperatureConstants)[4];
	v->csParam2 = REAL(SoilTemperatureConstants)[5];
	v->shParam = REAL(SoilTemperatureConstants)[6];
	v->Tsoil_constant = REAL(SoilTemperatureConstants)[7];
	v->stDeltaX = REAL(SoilTemperatureConstants)[8];
	v->stMaxDepth = REAL(SoilTemperatureConstants)[9];
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'soiltemp-constants'");
	#endif

	PROTECT(TranspirationRegions = GET_SLOT(SW_SIT, install("TranspirationRegions")));
	p_transp = INTEGER(TranspirationRegions);
	v->n_transp_rgn = nrows(TranspirationRegions);
	if (MAX_TRANSP_REGIONS < v->n_transp_rgn) {
		too_many_regions = TRUE;
	} else {
		for (i = 0; i < v->n_transp_rgn; i++) {
			_TranspRgnBounds[p_transp[i + v->n_transp_rgn * 0] - 1] = p_transp[i + v->n_transp_rgn * 1] - 1;
		}
	}
	if (too_many_regions) {
		LogError(logfp, LOGFATAL, "siteparam.in : Number of transpiration regions"
				" exceeds maximum allowed (%d > %d)\n", v->n_transp_rgn, MAX_TRANSP_REGIONS);
	}
	#ifdef RSWDEBUG
	if (debug) swprintf(" > 'transp-regions'");
	#endif

	/* check for any discontinuities (reversals) in the transpiration regions */
	for (r = 1; r < v->n_transp_rgn; r++) {
		if (_TranspRgnBounds[r - 1] >= _TranspRgnBounds[r]) {
			LogError(logfp, LOGFATAL, "siteparam.in : Discontinuity/reversal in transpiration regions.\n");
		}
	}

	#ifdef RSWDEBUG
	if (debug) swprintf(" ... done. \n");
	#endif

	UNPROTECT(11);
}
