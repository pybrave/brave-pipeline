
process metaphlan {
    publishDir "output/metaphlan/${meta.id}", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/metaphlan:4.1.1"
    tag "${meta.id}"
    input:
    tuple val(meta),path(reads)
    val(metaphlan_db_latest)
    val(metaphlan_db_index)

    output:
    tuple val(meta), path("*_profile.txt")   ,                emit: profile
    tuple val(meta), path("*.biom")          ,                emit: biom
    tuple val(meta), path('*.bowtie2out.txt'), optional:true, emit: bt2out

    script:
    def bowtie2out = "--bowtie2out ${meta.id}.bowtie2out.txt" 
    """
    metaphlan \
        --nproc $task.cpus \
        --input_type fastq \
        --ignore_usgbs \
        ${reads[0]},${reads[1]} \
        ${bowtie2out} \
        --biom ${meta.id}.biom \
        --output_file ${meta.id}_profile.txt \
        --bowtie2db $metaphlan_db_latest  \
        --index $metaphlan_db_index  
    """

    stub:
    """
    
    """
}
// docker run --user $(id -u):$(id -g) --rm -it registry.cn-hangzhou.aliyuncs.com/wybioinfo/metaphlan:4.1.1 bash

workflow{
    ch_input = channel.fromList(params.remove_host_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    metaphlan(ch_input,params.metaphlan_database.path,params.metaphlan_database.db_index )

}