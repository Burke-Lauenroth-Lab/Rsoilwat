/********************************************************/
/********************************************************/
/*	Source file: Sky.c
 Type: module - used by Weather.c
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the
 information about the sky.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <stdio.h>
#include <stdlib.h>

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"

#include "SOILWAT2/SW_Sky.h"
#include "rSW_Sky.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

extern SW_SKY SW_Sky;

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

SEXP onGet_SW_SKY() {
	int i;
	SW_SKY *v = &SW_Sky;
	SEXP swCloud,SW_SKY;
	SEXP Cloud;
	SEXP Cloud_names, Cloud_names_x, Cloud_names_y;
	char *x_names[] = { "SkyCoverPCT", "WindSpeed_m/s", "HumidityPCT", "Transmissivity", "SnowDensity_kg/m^3" };
	char *y_names[] = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
	RealD *p_Cloud;

	PROTECT(swCloud = MAKE_CLASS("swCloud"));
	PROTECT(SW_SKY = NEW_OBJECT(swCloud));
	PROTECT(Cloud = allocMatrix(REALSXP, 5, 12));
	p_Cloud = REAL(Cloud);
	for (i = 0; i < 12; i++) { //i=columns
		p_Cloud[0 + 5 * i] = v->cloudcov[i];
		p_Cloud[1 + 5 * i] = v->windspeed[i];
		p_Cloud[2 + 5 * i] = v->r_humidity[i];
		p_Cloud[3 + 5 * i] = v->transmission[i];
		p_Cloud[4 + 5 * i] = v->snow_density[i];
	}

	PROTECT(Cloud_names = allocVector(VECSXP, 2));
	PROTECT(Cloud_names_x = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++) {
		SET_STRING_ELT(Cloud_names_x, i, mkChar(x_names[i]));
	}
	PROTECT(Cloud_names_y = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++) {
		SET_STRING_ELT(Cloud_names_y, i, mkChar(y_names[i]));
	}
	SET_VECTOR_ELT(Cloud_names, 0, Cloud_names_x);
	SET_VECTOR_ELT(Cloud_names, 1, Cloud_names_y);
	setAttrib(Cloud, R_DimNamesSymbol, Cloud_names);

	SET_SLOT(SW_SKY,install("Cloud"),Cloud);

	UNPROTECT(6);
	return SW_SKY;
}

// function for creating the daily S4 class for daily values
SEXP onGet_SW_SKY_daily() {
  int i;
  SW_SKY *v = &SW_Sky;
  SEXP swCloud_daily, SW_SKY_daily;
  SEXP Cloud_daily;
  SEXP Cloud_daily_names, Cloud_daily_names_x, Cloud_daily_names_y;
  char *x_names_daily[] = {"HumidityPCT"};
  RealD *p_Cloud_daily;

  PROTECT(swCloud_daily = MAKE_CLASS("swCloud_daily"));
  PROTECT(SW_SKY_daily = NEW_OBJECT(swCloud_daily));
  PROTECT(Cloud_daily = allocMatrix(REALSXP, 1, 366));
  p_Cloud_daily = REAL(Cloud_daily);
  for (i = 0; i < 366; i++) { //i=columns
    p_Cloud_daily[0 + 1 * i] = v->r_humidity[5]; // change 5 to i when possible. Just using 5 as hard coded value so dont get error for incorrect index
  }

  PROTECT(Cloud_daily_names = allocVector(VECSXP, 2));
  PROTECT(Cloud_daily_names_x = allocVector(STRSXP, 1));
  for (i = 0; i < 1; i++) { // for setting "column" name. increment when adding windspeed and solar radiation
    SET_STRING_ELT(Cloud_daily_names_x, i, mkChar(x_names_daily[i]));
  }
  PROTECT(Cloud_daily_names_y = allocVector(STRSXP, 366));
  char i_string[8];
  for (i = 0; i < 366; i++) {
    sprintf(i_string, "%d", i);
    SET_STRING_ELT(Cloud_daily_names_y, i, mkChar(i_string));
  }
  SET_VECTOR_ELT(Cloud_daily_names, 0, Cloud_daily_names_x);
  SET_VECTOR_ELT(Cloud_daily_names, 1, Cloud_daily_names_y);
  setAttrib(Cloud_daily, R_DimNamesSymbol, Cloud_daily_names);

  SET_SLOT(SW_SKY_daily,install("Cloud_daily"),Cloud_daily);

  UNPROTECT(6);
  return SW_SKY_daily;
}

void onSet_SW_SKY(SEXP sxp_SW_SKY) {
	int i;
	SW_SKY *v = &SW_Sky;
	RealD *p_Cloud;
	RealD *p_Cloud_daily;
	PROTECT(sxp_SW_SKY);
	p_Cloud = REAL(GET_SLOT(sxp_SW_SKY,install("Cloud")));
	p_Cloud_daily = REAL(GET_SLOT(sxp_SW_SKY,install("Cloud_daily")));

	MyFileName = SW_F_name(eSky);

	for (i = 0; i < 12; i++) { //i=columns
		v->cloudcov[i] = p_Cloud[0 + 5 * i];
		v->windspeed[i] = p_Cloud[1 + 5 * i];
		v->r_humidity[i] = p_Cloud[2 + 5 * i];
		v->transmission[i] = p_Cloud[3 + 5 * i];
		v->snow_density[i] = p_Cloud[4 + 5 * i];
	}
	for (i = 0; i < 366; i++) { //i=columns
	  v->r_humidity[i] = p_Cloud_daily[0 + 1 * i]; // going through for daily values
	}
	UNPROTECT(1);
}
