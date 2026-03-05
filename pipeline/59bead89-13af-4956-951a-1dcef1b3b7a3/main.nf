include {remove_hosts} from './../../software/32493b81-e61d-4f80-9088-7f939402ca82/main.nf'
include {metaphlan} from './../../software/131f8806-35e3-4d7c-b234-f14a2119aaa7/main.nf'


workflow  {
    ch_input = channel.fromList(params.clean_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    ch_remove_hosts_reads = remove_hosts(ch_input)
    // ch_remove_hosts_reads.view()
    metaphlan(ch_remove_hosts_reads,params.metaphlan_database.path,params.metaphlan_database.db_index )

}