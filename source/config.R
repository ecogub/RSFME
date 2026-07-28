# Shared constants for the RSFME analysis pipeline.
# Source this file at the top of any script that uses these values.

# --- HBEF (Hubbard Brook Experimental Forest, Watershed 3) ---
HBEF_AREA       <- 42.4      # hectares
HBEF_SITE_CODE  <- 'w3'
HBEF_TARGET_WY  <- 2016L

# --- Plynlimon (Upper Hafren) ---
PLYN_AREA       <- 122        # hectares
PLYN_SITE_CODE  <- 'UHF'
PLYN_TARGET_WY  <- 2008L

# --- Ca conversion coefficient ---
# Derived from linear regression of Ca ~ spCond at HBEF W3, WY 2016.
# Ca (mg/L) = spCond * CA_SPCOND_SLOPE
CA_SPCOND_SLOPE <- 0.06284158

# --- Coarsening experiment settings ---
COARSEN_REPS    <- 100L

# --- HBEF sensor solute column names ---
HBEF_SOLUTES    <- c('IS_NO3', 'IS_spCond')
