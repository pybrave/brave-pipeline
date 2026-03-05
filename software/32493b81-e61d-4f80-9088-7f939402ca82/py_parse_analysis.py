
# "analysis_dict":db_dict,
# "database_dict":database_dict,
# "extra_dict":extra_dict
def parse_data(analysis_dict,database_dict,extra_dict):

    return {
        **analysis_dict,
        **database_dict,
        **extra_dict
    }