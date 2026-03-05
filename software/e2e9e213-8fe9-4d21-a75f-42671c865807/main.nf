process HUMANN_PROFILE{
    // conda "/ssd1/zyd/.conda/envs/metagenome"
    //container "192.168.3.60:5001/humann:3.6"
    //label "process_low"
    publishDir "output/humann", mode: 'symlink', overwrite: true
    containerOptions "--volume ${utility_mapping}:/home/jovyan/.conda/envs/humann3.9/lib/python3.12/site-packages/humann/data/misc"

    
    tag "$meta.id"
    // publishDir = [
    //     path: { "${params.outdir}/humann" },
    //     mode: 'copy',
    //     saveAs: { filename -> filename.equals('versions.yml') ? null : filename }
    // ]
    // debug true
    //pod = [volumeClaim: 'nfdata', mountPath: '/data/databases/humann/utility_mapping',subPath:"databases/humann/utility_mapping"]
    
    input:
    tuple val(meta), path(profile)
    
    path utility_mapping


    output:
    // path "${meta.id}/*"
    tuple val(meta),   path("${meta.id}/pathabundance/${meta.id}_pathabundance.rpkm.tsv"), emit:pathabundance_rpkm
    tuple val(meta),   path("${meta.id}/pathabundance/${meta.id}_pathabundance.rpkm_unstratified.tsv"), emit:pathabundance_uns_rpkm
    tuple val(meta),   path("${meta.id}/pathabundance/${meta.id}_pathabundance.rpkm_stratified.tsv"), emit:pathabundance_s_rpkm  

    tuple val(meta),   path("${meta.id}/genefamilies/${meta.id}_genefamilies.rpkm.tsv"), emit:genefamilies_rpkm
 
 


    tuple val(meta),   path("${meta.id}/${meta.id}.KO.rpkm.txt"), emit:KO   
    tuple val(meta),   path("${meta.id}/${meta.id}.KO.rpkm_stratified.txt"), emit:KO_S
    tuple val(meta),   path("${meta.id}/${meta.id}.KO.rpkm_unstratified.txt"), emit:KO_UNS

    tuple val(meta),   path("${meta.id}/rxn/${meta.id}.rxn.rpkm_unstratified.txt"), emit:rxn
    tuple val(meta),   path("${meta.id}/go/${meta.id}.GO.rpkm_unstratified.txt"), emit:GO
    tuple val(meta),   path("${meta.id}/level4ec/${meta.id}.EC.rpkm_unstratified.txt"), emit:EC
    tuple val(meta),   path("${meta.id}/pfam/${meta.id}.pfam.rpkm_unstratified.txt"), emit:pfam
    tuple val(meta),   path("${meta.id}/eggnog/${meta.id}.eggnog.rpkm_unstratified.txt"), emit:eggnog
    tuple val(meta),   path("${meta.id}/*/*")
    // tuple val(meta),   path("${meta.id}/${meta.id}.EC.txt"), emit:ec



    // tuple val(meta),   path("${meta.id}/humann_split_stratified_table/*_genefamilies_stratified.tsv") , emit:genefamilies_stratified
    // tuple val(meta),  path("${meta.id}/humann_split_stratified_table/*_genefamilies_unstratified.tsv") , emit:genefamilies_unstratified
    // tuple val(meta),  path("${meta.id}/humann_split_stratified_table/*_pathabundance_stratified.tsv") , emit:pathabundance_stratified
    // tuple val(meta),  path("${meta.id}/humann_split_stratified_table/*_pathabundance_unstratified.tsv") , emit:pathabundance_unstratified
 // humann_config --print
// humann_config --update  database_folders utility_mapping  /data/databases/humann/utility_mapping/
    // humann_config --update  database_folders utility_mapping ${utility_mapping}

    script:
    def genefamilies = profile[0]
    def pathabundance = profile[1]
    """
    export PATH=/home/jovyan/.conda/envs/humann3.9/bin:\$PATH
    humann_config --print
    [ ! -d ${meta.id} ] && mkdir -p ${meta.id}
    humann_split_stratified_table -i ${pathabundance} -o ${meta.id}/pathabundance
    humann_split_stratified_table -i ${genefamilies} -o ${meta.id}/genefamilies
    
    
    humann_renorm_table --input ${genefamilies} --units relab -s n --output ${meta.id}/genefamilies/${meta.id}_genefamilies.rpkm.tsv || true
    humann_renorm_table --input ${pathabundance} --units relab -s n --output ${meta.id}/pathabundance/${meta.id}_pathabundance.rpkm.tsv || true
 

    humann_split_stratified_table -i ${meta.id}/pathabundance/${meta.id}_pathabundance.rpkm.tsv -o ${meta.id}/pathabundance/  || true
    humann_split_stratified_table -i ${meta.id}/genefamilies/${meta.id}_genefamilies.rpkm.tsv -o ${meta.id}/genefamilies/  || true

    [ ! -d ${meta.id}/rxn ] && mkdir -p ${meta.id}/rxn
    [ ! -d ${meta.id}/go ] && mkdir -p ${meta.id}/go
    [ ! -d ${meta.id} ] && mkdir -p ${meta.id}
    [ ! -d ${meta.id}/level4ec ] && mkdir -p ${meta.id}/level4ec
    [ ! -d ${meta.id}/pfam ] && mkdir -p ${meta.id}/pfam
    [ ! -d ${meta.id}/eggnog ] && mkdir -p ${meta.id}/eggnog


	humann_regroup_table --input ${meta.id}_genefamilies.tsv -g uniref90_rxn      -u N -p N -o   ${meta.id}/rxn/${meta.id}.rxn.txt || true
	humann_regroup_table --input ${meta.id}_genefamilies.tsv -g uniref90_go       -u N -p N -o   ${meta.id}/go/${meta.id}.GO.txt || true
	humann_regroup_table --input ${meta.id}_genefamilies.tsv -g uniref90_ko       -u N -p N -o   ${meta.id}/${meta.id}.KO.txt || true
	humann_regroup_table --input ${meta.id}_genefamilies.tsv -g uniref90_level4ec -u N -p N -o   ${meta.id}/level4ec/${meta.id}.EC.txt || true
	humann_regroup_table --input ${meta.id}_genefamilies.tsv -g uniref90_pfam     -u N -p N -o   ${meta.id}/pfam/${meta.id}.pfam.txt || true
	humann_regroup_table --input ${meta.id}_genefamilies.tsv -g uniref90_eggnog   -u N -p N -o   ${meta.id}/eggnog/${meta.id}.eggnog.txt || true
    

    humann_renorm_table --input ${meta.id}/rxn/${meta.id}.rxn.txt        --units relab -s n --output     ${meta.id}/rxn/${meta.id}.rxn.rpkm.txt   || true
	humann_renorm_table --input ${meta.id}/go/${meta.id}.GO.txt          --units relab -s n --output     ${meta.id}/go/${meta.id}.GO.rpkm.txt  || true
	humann_renorm_table --input ${meta.id}/${meta.id}.KO.txt          --units relab -s n --output     ${meta.id}/${meta.id}.KO.rpkm.txt || true
	humann_renorm_table --input ${meta.id}/level4ec/${meta.id}.EC.txt    --units relab -s n --output     ${meta.id}/level4ec/${meta.id}.EC.rpkm.txt || true
	humann_renorm_table --input ${meta.id}/pfam/${meta.id}.pfam.txt      --units relab -s n --output     ${meta.id}/pfam/${meta.id}.pfam.rpkm.txt  || true
	humann_renorm_table --input ${meta.id}/eggnog/${meta.id}.eggnog.txt  --units relab -s n --output     ${meta.id}/eggnog/${meta.id}.eggnog.rpkm.txt || true



	humann_split_stratified_table -i ${meta.id}/rxn/${meta.id}.rxn.rpkm.txt         -o ${meta.id}/rxn || true
	humann_split_stratified_table -i ${meta.id}/go/${meta.id}.GO.rpkm.txt           -o ${meta.id}/go || true
	humann_split_stratified_table -i ${meta.id}/${meta.id}.KO.rpkm.txt           -o ${meta.id}  || true
	humann_split_stratified_table -i ${meta.id}/level4ec/${meta.id}.EC.rpkm.txt     -o ${meta.id}/level4ec || true
	humann_split_stratified_table -i ${meta.id}/pfam/${meta.id}.pfam.rpkm.txt       -o ${meta.id}/pfam || true
	humann_split_stratified_table -i ${meta.id}/eggnog/${meta.id}.eggnog.rpkm.txt   -o ${meta.id}/eggnog || true




    """

}


process HUMANN_KEGG{
    container "192.168.3.60:5001/pandas:1.5.1"
    label "process_low"
    tag "$meta.id"
    publishDir "output/humann", mode: 'symlink', overwrite: true


    input:
    tuple val(meta), path(ko)
    path module_ko
    path pathway_ko


    output:
    // path "*"
    tuple val(meta),   path("${meta.id}/${meta.id}.Module.txt"), emit:module
    tuple val(meta),   path("${meta.id}/${meta.id}.Pathway.txt"), emit:pathway
    script:
    """
    [ ! -d ${meta.id} ] && mkdir -p ${meta.id}
    [ ! -d ${meta.id} ] && mkdir -p ${meta.id}

    kegg.py ${module_ko}  ${ko} ${meta.id}/${meta.id}.Module.txt
    kegg.py ${pathway_ko} ${ko} ${meta.id}/${meta.id}.Pathway.txt
    sed -i 's/\\t//g'   ${meta.id}/${meta.id}.Module.txt
    sed -i 's/\\r//g'   ${meta.id}/${meta.id}.Module.txt
    sed -i 's/,/\\t/g'  ${meta.id}/${meta.id}.Module.txt
    sed -i 's/\\t//g'   ${meta.id}/${meta.id}.Pathway.txt
    sed -i 's/\\r//g'   ${meta.id}/${meta.id}.Pathway.txt
    sed -i 's/,/\\t/g'  ${meta.id}/${meta.id}.Pathway.txt
    """
}
process GMM{
    tag "$meta.id"
    publishDir "output/humann", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/gmm"
    input:
    tuple val(meta), path(normalization)
    path txt


    output:
    tuple val(meta), path("${meta.id}/GMM/${meta.id}*")    , emit: profile 

    script:
    def prefix = task.ext.prefix ?: "${meta.id}"
    
    """
    gmm.sh -d ${txt} -i ${normalization} -s average -o ${meta.id}/GMM
    """
    // cp ${meta.id}/gene_profile.modules ${meta.id}/${meta.id}_gene_profile.modules
}
process GBM{
    tag "$meta.id"
    publishDir "output/humann", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/gmm"
    input:
    tuple val(meta), path(normalization)
    path txt


    output:
    tuple val(meta), path("${meta.id}/GBM/${meta.id}*")    , emit: profile 

    script:
    def prefix = task.ext.prefix ?: "${meta.id}"
    
    """
    gmm.sh -d ${txt} -i ${normalization} -s average -o ${meta.id}/GBM
    """
    // cp ${meta.id}/gene_profile.modules ${meta.id}/${meta.id}_gene_profile.modules
}

process anno_kegg{
    tag "$meta.id"
    debug true
    publishDir "output/humann", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/maaslin2:1.22	"
    containerOptions "--volume ${params.pipeline_dir}:/brave"
    input:
    tuple val(meta), path(profile)

    script:
    
    """
    echo "111111111111111111111111111112222233333222222"
    Rscript /brave/7530139e-8985-423f-9fb6-32650828ca40/script/4e300062-32d3-47d0-a1f0-0374a268f80f/main.R
    """
}
workflow{
    ch_input = channel.fromList(params.humann_profile).map(it->[[id:it.sample_name],[it.genefamilies,it.pathabundance]])
    anno_kegg(ch_input)
     //ch_input.view()
    // HUMANN_PROFILE(ch_input,params.humann_utility_mapping.path)
    //HUMANN_KEGG(HUMANN_PROFILE.out.KO_UNS,"/data/databases/kegg/module_ko.new.txt","/data/databases/kegg/pathway_ko.new.txt")
    //GMM(HUMANN_PROFILE.out.KO_UNS,"/data/metagenomics/pml_out/GMMs.v1.07.txt")

    //GBM(HUMANN_PROFILE.out.KO_UNS,"/data/metagenomics/pml_out/GBM.inputfile.txt")


}