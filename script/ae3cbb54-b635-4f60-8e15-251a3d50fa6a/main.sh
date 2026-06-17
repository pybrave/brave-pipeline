# bash

cd output

ln -s  "{{bed}}" .
ln -s  "{{bim}}"  .
ln -s  "{{fam}}"  .
ln -s   "{{covar}}" .
ln -s   "{{pheno.path}}" pheno

# name=$(basename -s .bed {{bed}})


# /data2/brave_analysis_workspace/package/code_server/software/plink/plink \
#   --bfile ${name} \
#   --pca 20 \
#   --out ${name}

# ln -s ${name}.eigenval ${name}.eigenval.tsv
# awk '{$1=$1}1' OFS='\t' ${name}.eigenvec > ${name}.eigenvec.tsv


# cat > {{output_dir}}/outputs.json <<EOF
# {
#   "eigenval": "{{output_dir}}/${name}.eigenval",
#   "eigenvec": "{{output_dir}}/${name}.eigenvec"
# }
# EOF


/data2/brave_analysis_workspace/package/code_server/software/plink/plink \
  --bfile 890S.sampleQc.snpQC \
  --pheno pheno \
  --logistic \
  --covar 890S.sampleQc.snpQC.eigenvec  \
  --out gwas


head gwas.assoc.logistic | awk 'BEGIN {OFS="\t"} {$1=$1; print}' - > gwas.assoc.logistic.head.tsv
