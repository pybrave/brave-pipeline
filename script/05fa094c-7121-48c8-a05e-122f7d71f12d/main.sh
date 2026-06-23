# bash


cd output
ln -s  "{{map.path}}"  {{map.file_name}}
ln -s  "{{ped.path}}"   {{ped.file_name}}

{% if keep %}
ln -s  "{{keep.path}}"  {{keep.file_name}}

/data2/brave_analysis_workspace/package/code_server/software/plink/plink \
  --file "$(basename -s .map {{map.file_name}})" \
  --keep {{keep.file_name}} \
  --make-bed \
  --out 890S

{% else %}


/data2/brave_analysis_workspace/package/code_server/software/plink/plink \
  --file "$(basename -s .map {{map.file_name}})" \
  --make-bed \
  --out 890S


{% endif %}




cat > {{output_dir}}/outputs.json <<EOF
{
  "bed": "{{output_dir}}/890S.bed",
  "bim":"{{output_dir}}/890S.bim",
  "fam":"{{output_dir}}/890S.fam"
}
EOF