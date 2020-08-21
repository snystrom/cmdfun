sed -i -e 's/cmd_args_to_flags/cmd_list_interp/g' $1
sed -i -e 's/cmd_list_crystallize/cmd_list_to_flags/g' $1
sed -i -e 's/cmd_path_handle/cmd_path_search/g' $1
sed -i -e 's/path_handler/path_search/g' $1
sed -i -e 's/handler/search_function/g' $1
# manually : handle -> search
