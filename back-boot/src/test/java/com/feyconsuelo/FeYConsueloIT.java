package com.feyconsuelo;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import com.jayway.jsonpath.JsonPath;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
@Slf4j
public class FeYConsueloIT {

    @Autowired
    private MockMvc mockMvc;

    public static String asJsonString(final Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    @Order(1)
    void musicianCrudTest() throws Exception {

        final MvcResult postResult = this.mockMvc.perform(MockMvcRequestBuilders
                        .post("/musician")
                        .content(
                                asJsonString(
                                        MusicianRequestDto.builder()
                                                .dni("dni")
                                                .name("name")
                                                .surname("surname")
                                                .direction("direction")
                                                .municipality("municipality")
                                                .province("province")
                                                .email("email")
                                                .voiceId(1L)
                                                .image("")
                                                .build()
                                )
                        )
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated())
                .andExpect(MockMvcResultMatchers.jsonPath("$.dni").exists())
                .andReturn();

        final String id = JsonPath.parse(postResult.getResponse().getContentAsString()).read("$.id");

        this.mockMvc.perform(MockMvcRequestBuilders.get("/musician/{id}", id))
                .andExpect(status().isOk())
                .andExpect(MockMvcResultMatchers.jsonPath("$.dni").exists());

        this.mockMvc.perform(MockMvcRequestBuilders
                        .put("/musician")
                        .content(
                                asJsonString(
                                        MusicianRequestDto.builder()
                                                .dni("dni")
                                                .name("name")
                                                .surname("surname")
                                                .direction("direction")
                                                .municipality("municipality")
                                                .province("province")
                                                .email("email")
                                                .voiceId(1L)
                                                .image("")
                                                .build()
                                )
                        )
                        .contentType(MediaType.APPLICATION_JSON)
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(MockMvcResultMatchers.jsonPath("$.dni").exists());

        log.info("Deleting id: {}", id);

        this.mockMvc.perform(MockMvcRequestBuilders.delete("/musician/{id}", id))
                .andExpect(status().isNoContent());
    }

}
