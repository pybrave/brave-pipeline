
process metawrap_assembly{
    input:
    tuple val(meta),path(reads)
    output:
    // tuple val(meta),path("${meta.id}/scaffolds.fasta"), emit:scaffolds
    
    tuple val(meta),path("*/final_assembly.fasta"), emit: fasta
    tuple val(meta),path("*/*")
    
    script:
    def reads1 = reads[0].toString().replace(".gz","")
    def reads2 = reads[1].toString().replace(".gz","")

    """
    #gunzip -c ${reads[0]} > ${reads1}
    #gunzip -c ${reads[1]} > ${reads2}
    #export PATH=/home/jovyan/.conda/envs/metawrap-env/bin:/ssd1/wy/workspace2/software/metaWRAP/bin:\$PATH
   # metawrap assembly \
    #    -1 ${reads[0]} \
    #    -2 ${reads[1]} \
    #    -m ${task.memory.toGiga()} -t ${task.cpus} \
    #    --metaspades -o ${meta.id}
    sleep 2
    mkdir ${meta.id}
    touch ${meta.id}/final_assembly.fasta
    """
}
workflow{
    
    ch_input = channel.fromList(params.fastp_clean_reads).map(it->[[id:it.analysis_key],[it.fastq1,it.fastq2]])
    ch_input.view()
    ch_input.view()
    ch_metawrap_assembly_res =  metawrap_assembly(ch_input)

}