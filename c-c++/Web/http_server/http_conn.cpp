#include "http_conn.h"

const char* ok_200_title = "OK";
const char* error_400_title = "Bad Request";
const char* error_400_form = "Your request has bad syntax or inherently impossible to satisfy.\n";
const char* error_403_title = "Forbidden";
const char* error_403_form = "You do not have permission to get file from this server.\n";
const char* error_404_title = "No Found";
const char* error_404_form = "The request file was not found on this server.\n";
const char* error_500_title = "Internal Error";
const char* error_500_form = "There was an unusual problem serving the request file.\n";

const char* doc_root = "/var/www/html";
const char* host_index = "index.html";

int http_conn::m_user_count = 0;

string tostring(int number)
{
    string s = "";
    while(number)
    {
        s += (number%10) + '0';
        number /= 10;
    }
    reverse(s.begin(),s.end());
    return s;
}

void http_conn::close_conn(bool real_close)
{
    if(real_close && (m_sockfd != -1))
    {
        removefd(m_epollfd,m_sockfd);
        m_sockfd = -1;
        m_user_count--;
    }
}

void http_conn::init(int epollfd,int sockfd,const sockaddr_in& addr)
{
    m_epollfd= epollfd;
    m_sockfd = sockfd;
    m_address = addr;

    //int reuse = 1;
    //setsockopt(m_sockfd,SOL_SOCKET,SO_REUSEADDR,&reuse,sizeof(reuse));
    addfd(m_epollfd,sockfd);//,true);
    m_user_count++;
    
    init();
}

void http_conn::init()
{
    m_read_idx = 0;
    memset(m_read_buf,0,READ_BUFFER_SIZE);

    m_check_state = CHECK_STATE_REQUESTLINE;
    m_linger = false;
    m_method = GET;
    m_url = 0;
    m_version = 0;
    m_content_length = 0;
    m_host = 0;
    m_start_line = 0;
    m_checked_idx = 0;
 
    bytes_to_send = 0;
    bytes_have_send = 0;
    m_write_idx = 0;
    memset(m_write_buf,0,WRITE_BUFFER_SIZE);
    memset(m_real_file,0,FILENAME_LEN);
}

//--------------------------------------------------------------------------------------------------------
//  parse request ...

http_conn::LINE_STATUS http_conn::parse_line()
{
    char temp;
    for(;m_checked_idx < m_read_idx; ++m_checked_idx)
    {
        temp = m_read_buf[m_checked_idx];
        if(temp == '\r')
        {
            if((m_checked_idx + 1) == m_read_idx)
            {
                return LINE_OPEN;
            }
            else if(m_read_buf[m_checked_idx + 1] == '\n')
            {
                m_read_buf[m_checked_idx++] = '\0';
                m_read_buf[m_checked_idx++] = '\0';
                return LINE_OK;
            }
            return LINE_BAD;
        }
        else if(temp == '\n')
        {
            if((m_checked_idx > 1) && (m_read_buf[m_checked_idx - 1] == '\r') )
            {
                m_read_buf[m_checked_idx-1] = '\0';
                m_read_buf[m_checked_idx++] = '\0';
                return LINE_OK;
            }
            return LINE_BAD;
        }
    }
    return LINE_OPEN;
}

bool http_conn::read()
{
    if(m_read_idx >= READ_BUFFER_SIZE)
    {
        return false;
    }
    int bytes_read = 0;
    while (true)
    {
        bytes_read = recv(m_sockfd,m_read_buf+m_read_idx,READ_BUFFER_SIZE - m_read_idx,0);
        if(bytes_read == -1)
        {
            if(errno == EAGAIN || errno == EWOULDBLOCK)
            {
                printf("read()-->%s",(errno==EAGAIN)?"EAGAIN":"EWOULDBLOCK");
                break;
            }
            return false;
        }
        else if(bytes_read == 0)
        {
            return false;
        }
        m_read_idx += bytes_read;
    }
    return true;
}
char* http_conn::split_str(char *text)
{
    int i;
    for(i = 0 ; text[i] ; i++)
    {
        if(text[i] == ' '||text[i] == '\t')
            break;
    }
    if(!text[i]) return text+i;
    while(text[i] == ' '||text[i] == '\t') text[i++]='\0';
    return text+i;
}
http_conn::HTTP_CODE http_conn::parse_request_line(char* text)
{
    /* // for testing '\t' or ' '
    for(int i = 0;text[i];i++)
        if(text[i] == '\t')
            printf("(%d %c)",i,'t');
        else if(text[i] == ' ')
            printf("(%d %c)",i,'s');
    */
    char* method = text;
    m_url = split_str(method);
    printf("method ---> %s\n",method);
    if(strcasecmp(method,"GET") == 0)
    {
        m_method = GET;
    }
    else 
    {
        return BAD_REQUEST;
    }
    
    if(!m_url)
    {
        return BAD_REQUEST;
    }
    printf("url --- > %s\n",m_url);    
    m_version = split_str(m_url);
    if(!m_version)
    {
        return BAD_REQUEST;
    }
    
    if(strcasecmp(m_version,"HTTP/1.1") != 0)
    {
        return BAD_REQUEST;
    }
    printf("http_version ---- >%s\n",m_version);
    if(strncasecmp(m_url,"http://",7) == 0)
    {
        m_url += 7;
        m_url = strchr(m_url,'/');
    }
    if(!m_url || m_url[0] != '/')
    {
        return BAD_REQUEST;
    }
    m_check_state = CHECK_STATE_HEADER;
    return NO_REQUEST;
}

http_conn::HTTP_CODE http_conn::parse_headers(char* text)
{
    if(text[0] == '\0')// blank_line
    {
        if(m_content_length != 0)
        {
            m_check_state = CHECK_STATE_CONTENT;
            return NO_REQUEST;
        }
        return GET_REQUEST;
    }

    else if(strncasecmp(text,"Connection:",11) == 0)
    {
        text = split_str(text);
        if(strcasecmp(text,"keep-alive") == 0)
        {
            printf("linger--------->true");
            m_linger = true;
        }
    }
    else if(strncasecmp(text,"Content-Length:",15) == 0)
    {
        text = split_str(text);
        m_content_length = atol(text);
    }
    else if(strncasecmp(text,"Host:",5) == 0)
    {
        text = split_str(text);
        m_host = text;
        printf("host------>%s",m_host);
    }
    else
    {
        printf("oop! unknow header %s\n",text);
    }
    return NO_REQUEST;
}

http_conn::HTTP_CODE http_conn::parse_content(char* text)
{
    if(m_read_idx >= (m_content_length + m_checked_idx))
    {
        text[m_content_length] = '\0';
        return GET_REQUEST;
    }
    return NO_REQUEST;
}

http_conn::HTTP_CODE http_conn::process_read()
{
    LINE_STATUS line_status = LINE_OK;
    HTTP_CODE ret = NO_REQUEST;
    char* text = 0;

    while(  ((m_check_state == CHECK_STATE_CONTENT) && (line_status == LINE_OK)) || ((line_status = parse_line()) == LINE_OK) )
    {
        text = get_line();
        m_start_line = m_checked_idx;
        printf("got 1 http line: %s\n",text);

        switch(m_check_state)
        {
            case CHECK_STATE_REQUESTLINE:
                {
                    ret = parse_request_line(text);
                    if(ret == BAD_REQUEST)
                    {
                        return BAD_REQUEST;
                    }
                    break;
                }
            case CHECK_STATE_HEADER:
                {
                    ret = parse_headers(text);
                    if(ret == BAD_REQUEST)
                    {
                        return BAD_REQUEST;
                    }
                    else if(ret == GET_REQUEST)
                    {
                        return do_request();
                    }
                    break;
                }
            case CHECK_STATE_CONTENT:
                {
                    ret = parse_content(text);
                    if(ret == GET_REQUEST)
                    {
                        return do_request();
                    }
                    line_status = LINE_OPEN;
                    break;
                }
            default:
                {
                    return INTERNAL_ERROR;
                }

        }
        
    }
    return NO_REQUEST;
}


http_conn::HTTP_CODE http_conn::do_request()
{
    strcpy(m_real_file,doc_root);
    int len = strlen(doc_root);
    //if(strcmp(m_url,"/") == 0)
    //    strcat()
    strncpy(m_real_file + len,m_url,FILENAME_LEN - len - 1);
    if(strcmp(m_url,"/") == 0)
        strncpy(m_real_file + len + 1,host_index,FILENAME_LEN - len -2);
    
    printf("----------++++++ %s\n",m_real_file);

    if( stat(m_real_file,&m_file_stat) < 0)
    {
        printf("ret -------> %s\n","no_resource");
        return NO_RESOURCE;
    }
    if( !(m_file_stat.st_mode & S_IROTH) )
    {
        printf("ret -------> %s\n","forbidden_request");
        return FORBIDDEN_REQUEST;
    }
    if(S_ISDIR(m_file_stat.st_mode))
    {
        printf("ret -------> %s\n","bad_request");
        return BAD_REQUEST;
    }
    int fd = open(m_real_file,O_RDONLY);
    m_file_address = (char*) mmap(0,m_file_stat.st_size,PROT_READ,MAP_PRIVATE,fd,0);
    close(fd);
    return FILE_REQUEST;
}

void http_conn::unmap()
{
    if(m_file_address)
    {
        munmap(m_file_address,m_file_stat.st_size);
        m_file_address = 0;
    }
}

//-----------------------------------------------------------------------------------------------------------------
// response .....
bool http_conn::write()
{
    printf("\nresponse :\n %s\n",m_write_buf);
    int temp = 0;
    //int bytes_have_send = 0;
    //int bytes_to_send = m_write_idx;
    if(bytes_to_send == 0)
    {
        modfd(m_epollfd,m_sockfd,EPOLLIN);
        init();
        return true;
    }
    while(1)
    {
        temp = writev(m_sockfd,m_iv,m_iv_count);
        if(temp <= -1)
        {
            if(errno == EAGAIN)
            {
                modfd(m_epollfd,m_sockfd,EPOLLOUT);
                return true;
            }
            unmap();
            return false;
        }
        //bytes_to_send -= temp;
        bytes_have_send += temp;
        printf("to_send %d, have_send %d\n",bytes_to_send,bytes_have_send);
        if(bytes_to_send <= bytes_have_send)
        {
            unmap();
            if(m_linger)
            {
                init();
                modfd(m_epollfd,m_sockfd,EPOLLIN);
                return true;
            }
            else
            {
                modfd(m_epollfd,m_sockfd,EPOLLIN);
                return false;
            }
        }
    }    
}

bool http_conn::add_response(string response)
{
    if(m_write_idx >= WRITE_BUFFER_SIZE)
    {
        return false;
    }
    int len = response.length();
    if(m_write_idx + len > WRITE_BUFFER_SIZE )
    {
        return false;
    }
    for(int i = 0;i < len ; ++i )
    {
        m_write_buf[m_write_idx++] = response[i]; 
    }
    return true;
}

bool http_conn::add_status_line(int status,const char* title)
{
    string s = "HTTP/1.1 ";
    //char num[10];
    //itoa(status,num,10);
    cout << "s=" << s <<endl;
    s += tostring(status);
    s += " ";
    cout << "s=" << s <<endl;
    s += title;
    s += "\r\n";
    cout << "s=" << s <<endl;
    return add_response(s);
}

bool http_conn::add_headers(int content_len)
{
    bool flag = true;
    flag = flag & add_content_length(content_len);
    flag = flag & add_linger();
    flag = flag & add_blank_line();
    return flag;
}

bool http_conn::add_content_length(int content_len)
{
    string s = "";
    //char num[10];
    s += "Content-Length: ";
    //itoa(content_len,num,10);
    s += tostring(content_len);
    s += "\r\n";    
    return add_response(s); 
}
bool http_conn::add_linger()
{
    string s = "";
    s += "Connection: ";
    (m_linger == true) ? s += "keep-alive" : s += "close";
    s += "\r\n";
    return add_response(s);
}
bool http_conn::add_blank_line()
{
    string s = "\r\n";
    return add_response(s);
}

bool http_conn::add_content(const char* content)
{
    string s = content;
    return add_response(s);
}

bool http_conn::process_write(HTTP_CODE ret)
{
    
    switch(ret)
    {
        case INTERNAL_ERROR:
            {printf("500\n");
                add_status_line(500,error_500_title);
                add_headers(strlen(error_500_form));      
                if( !add_content(error_500_form) )
                {
                    return false;
                }
                break;
            }
        case BAD_REQUEST:
            {printf("400\n");
                add_status_line(400,error_400_title);
                add_headers(strlen(error_400_form));
                if(!add_content(error_400_form))
                {
                    return false;
                }
                break;
            }
        case NO_RESOURCE:
            {printf("404\n");
                add_status_line(404,error_404_title);
                add_headers(strlen(error_404_form));
                if(!add_content(error_404_form))
                {
                    return false;
                }
                break;
            }
           case FORBIDDEN_REQUEST:    
            {printf("403\n");
                add_status_line(403,error_403_title);
                add_headers(strlen(error_403_form));
                if(!add_content(error_403_form))
                {
                    return false;
                }
                break;
            }
           case FILE_REQUEST:
            {printf("200\n");
                printf("start add status ...\n");
                add_status_line(200,ok_200_title);
                printf("add status ok...\n");
                if(m_file_stat.st_size != 0)
                {
                    add_headers(m_file_stat.st_size);
                    printf("add header ok ...\n");
                    m_iv[0].iov_base = m_write_buf;
                    m_iv[0].iov_len = m_write_idx;
                    m_iv[1].iov_base = m_file_address;
                    m_iv[1].iov_len = m_file_stat.st_size;
                    m_iv_count = 2;
                    bytes_to_send = m_write_idx + m_file_stat.st_size;
                    return true;
                }
                else
                {
                    const char* ok_string = "<html><body> hehe </body></html>";
                    add_headers(strlen(ok_string));
                    if(!add_content(ok_string))
                    {
                        return false;
                    }
                }
            }
           default:
            {
                return false;
            }
    }
    bytes_to_send = m_write_idx;
    m_iv[0].iov_base = m_write_buf;
    m_iv[0].iov_len = m_write_idx;
    m_iv_count = 1;
    return true;
}

void http_conn::process()
{
    
    HTTP_CODE read_ret = process_read();

    if(read_ret == NO_REQUEST)
    {
        return ;
    }

    bool write_ret = process_write(read_ret);
    if(!write_ret)
    {
        close_conn();
        return ;
    }
    modfd(m_epollfd,m_sockfd,EPOLLOUT);
}





