	integer ISF_LINE_LEN
	integer ISF_NULL			
	integer ISF_COMM_LEN        
	integer ISF_EVID_LEN
	integer ISF_REGION_LEN
	integer ISF_ETYPE_LEN 
	integer ISF_AUTHOR_LEN
	integer ISF_ORIGID_LEN     
	integer ISF_MAGTYPE_LEN
	integer ISF_STA_LEN
	integer ISF_NET_LEN
	integer ISF_CHAN_LEN
	integer ISF_PHASE_LEN
	integer ISF_ARRID_LEN
	integer ISF_F_TYPE_LEN
	integer ISF_F_PLANE_LEN
	integer ISF_I_LOCTYPE_LEN
	integer ISF_COUNTRY_LEN
	integer ISF_POSTCODE_LEN
	integer ISF_I_SCALE_LEN

	integer ISF_NUM_STA
	integer ISF_NUM_PARAM

	parameter (ISF_LINE_LEN=140)
	parameter (ISF_COMM_LEN=80)
	parameter (ISF_EVID_LEN=8)
	parameter (ISF_REGION_LEN=65)
	parameter (ISF_ETYPE_LEN=2)
	parameter (ISF_AUTHOR_LEN=9)
	parameter (ISF_ORIGID_LEN=8)
	parameter (ISF_MAGTYPE_LEN=5)
	parameter (ISF_STA_LEN=5)
	parameter (ISF_NET_LEN=9)
	parameter (ISF_CHAN_LEN=3)
	parameter (ISF_PHASE_LEN=8)
	parameter (ISF_ARRID_LEN=8)
	parameter (ISF_F_TYPE_LEN=3)
	parameter (ISF_F_PLANE_LEN=5)
	parameter (ISF_I_LOCTYPE_LEN=6)
	parameter (ISF_COUNTRY_LEN=3)
	parameter (ISF_POSTCODE_LEN=10)
	parameter (ISF_I_SCALE_LEN=5)

	parameter (ISF_NUM_STA=200)
	parameter (ISF_NUM_PARAM=100)

	parameter (ISF_NULL=-999.)

	character isf_bulletin_error*(ISF_LINE_LEN)
	character isf_prev_line_type*(ISF_LINE_LEN)
	common /isf_error/ isf_bulletin_error
	common /isf_write/ isf_prev_line_type
