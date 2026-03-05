
process  humann{
    publishDir "output/humann", mode: 'symlink', overwrite: true
    input:
    tuple val(meta),path(reads),path(profile)
    val(nucleotide_database)
    val(protein_database)


    output:
    tuple val(meta), path("${meta.id}/*"), emit: all
    tuple val(meta),path("${meta.id}/*pathabundance.tsv"), emit:pathabundance
    tuple val(meta),path("${meta.id}/*genefamilies.tsv"), emit:genefamilies
    tuple val(meta),path("${meta.id}/*pathcoverage.tsv"), emit:pathcoverage

    script:
    """
    cat $reads >  ${meta.id}.clean.gz
    export PATH=/home/jovyan/.conda/envs/humann3.9/bin:\$PATH
    humann --nucleotide-database ${nucleotide_database} \
        --protein-database ${protein_database} \
        --input ${meta.id}.clean.gz    \
        --threads ${task.cpus}  \
        --taxonomic-profile ${profile} \
        -o ${meta.id}
    rm ${meta.id}.clean.gz
    mv ${meta.id}/${meta.id}_humann_temp/${meta.id}_bowtie2_aligned.tsv ${meta.id}
    mv ${meta.id}/${meta.id}_humann_temp/${meta.id}_diamond_aligned.tsv  ${meta.id}
    mv ${meta.id}/${meta.id}_humann_temp/${meta.id}.log  ${meta.id}
    rm -rf ${meta.id}/${meta.id}_humann_temp
    """

    stub:
    """
    
    """
}


workflow{
    ch_fastq_input = channel.fromList(params.remove_host_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    ch_profile_input = channel.fromList(params.metaphlan_abundance).map(it->[[id:it.sample_name],it.profile])
    ch_input = ch_fastq_input.join(ch_profile_input, by:0)
    //ch_fastq_input.combine(ch_profile_input, by: 0).view()
    humann(ch_input, params.humann_nucleotide_database.path, params.humann_protein_database.path )

}